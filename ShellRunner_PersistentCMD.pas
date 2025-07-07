unit ShellRunner_PersistentCMD;

interface

uses
  System.SysUtils,
  Winapi.Windows,
  System.Classes,
  System.Threading,
  System.IOUtils, // Added for TPath
  AgentServiceI;


type
  TShellRunnerPersistentCMD = class(TInterfacedObject, IShellRunner)
  private
    FOnOutput: TOutputProc;
    FOnError: TOutputProc;
    FOnExit: TExitCodeProc;
    FProcessHandle: THandle;
    FThread: TThread;
    FStdInWrite: THandle;
    FStdOutRead: THandle;
    FStopRequested: Boolean;
    FLock: TObject;
    FEnvInfo: IEnvironmentInfo;
    FCurrentDir: string;
    /// Per inviare comandi in mutua esclusione
    procedure InternalWriteCommand(const Cmd: string);
    /// Tries to track the current directory based on 'cd' commands
    procedure TrackCurrentDirectory(const Cmd: string);
  public
    constructor Create(const Params: IEnvironmentInfo);
    destructor Destroy; override;

    // --- IShellRunner implementation ---
    procedure ExecuteCommand(
      const Command: string;
      const OnOutput: TOutputProc;
      const OnError: TOutputProc;
      const OnExit: TExitCodeProc
    );
    procedure StopExecution;
    function IsRunning: Boolean;
    function GetWorkingDirectory: string;
    procedure SetWorkingDirectory(const Dir: string);
  end;

  // Reader Thread Class
  TReaderThread = class(TThread)
  private
    FOwner: TShellRunnerPersistentCMD;
  protected
    procedure Execute; override;
  public
    constructor Create(const AOwner: TShellRunnerPersistentCMD);
  end;

implementation

{ TShellRunnerPersistentCMD }

constructor TShellRunnerPersistentCMD.Create(const Params: IEnvironmentInfo);
var
  SecurityAttr: TSecurityAttributes;
  StdOutWrite, StdInRead: THandle;
  StartInfo: TStartupInfo;
  ProcInfo: TProcessInformation;
  CmdLine: string;
  PCmdLine: array[0..1023] of Char;
  HomeDir: string;
begin
  inherited Create;
  FStopRequested := False;
  FLock := TObject.Create;
  FEnvInfo := Params;

  // Set initial working directory
  HomeDir := FEnvInfo.GetHomeDir.Trim;
  if (HomeDir <> '') and DirectoryExists(HomeDir) then
    FCurrentDir := HomeDir
  else
    FCurrentDir := GetCurrentDir;

  // Prepare pipes for stdin/stdout
  FillChar(SecurityAttr, SizeOf(SecurityAttr), 0);
  SecurityAttr.nLength := SizeOf(SecurityAttr);
  SecurityAttr.bInheritHandle := True;

  if not CreatePipe(StdInRead, FStdInWrite, @SecurityAttr, 0) then
    raise Exception.Create('Unable to create stdin pipe');
  if not CreatePipe(FStdOutRead, StdOutWrite, @SecurityAttr, 0) then
    raise Exception.Create('Unable to create stdout pipe');

  // Ensure parent process handles are not inherited by the child
  SetHandleInformation(FStdInWrite, HANDLE_FLAG_INHERIT, 0);
  SetHandleInformation(FStdOutRead, HANDLE_FLAG_INHERIT, 0);

  // Set up STARTUPINFO to redirect standard handles
  ZeroMemory(@StartInfo, SizeOf(StartInfo));
  StartInfo.cb := SizeOf(StartInfo);
  StartInfo.dwFlags := STARTF_USESTDHANDLES or STARTF_USESHOWWINDOW;
  StartInfo.hStdInput := StdInRead;
  StartInfo.hStdOutput := StdOutWrite;
  StartInfo.hStdError := StdOutWrite;
  StartInfo.wShowWindow := SW_HIDE;

  // Command to start cmd.exe in interactive mode
  CmdLine := 'cmd.exe /k echo CMD Session Started';
  StrPCopy(PCmdLine, CmdLine);

  if not CreateProcess(
    nil,
    PCmdLine,
    nil,
    nil,
    True, // Inherit handles
    CREATE_NO_WINDOW,
    nil,
    PChar(FCurrentDir), // Set initial working directory
    StartInfo,
    ProcInfo
  ) then
  begin
    CloseHandle(StdInRead);
    CloseHandle(FStdInWrite);
    CloseHandle(FStdOutRead);
    CloseHandle(StdOutWrite);
    raise Exception.Create('Unable to start persistent cmd.exe: ' + SysErrorMessage(GetLastError));
  end;

  // Close handles no longer needed by the parent process
  CloseHandle(StdInRead);
  CloseHandle(StdOutWrite);
  CloseHandle(ProcInfo.hThread);

  FProcessHandle := ProcInfo.hProcess;

  // Start the reader thread
  FThread := TReaderThread.Create(Self);
end;

destructor TShellRunnerPersistentCMD.Destroy;
begin
  try
    if IsRunning then
      StopExecution;
  finally
    FreeAndNil(FLock);
    inherited;
  end;
end;

procedure TShellRunnerPersistentCMD.ExecuteCommand(
  const Command: string;
  const OnOutput: TOutputProc;
  const OnError: TOutputProc;
  const OnExit: TExitCodeProc
);
begin
  if FStopRequested or (FProcessHandle = 0) then
  begin
    OnError('Shell is not running.');
    Exit;
  end;

  // Set callbacks for this command execution
  FOnOutput := OnOutput;
  FOnError := OnError;
  FOnExit := OnExit;

  // Send the command to stdin, appending a marker to detect when it's finished.
  InternalWriteCommand(Format('%s & echo __CMD_END__', [Command]));
end;

procedure TShellRunnerPersistentCMD.StopExecution;
  begin
  // 1. Segnala a tutti i loop di fermarsi.
  // Questo previene l'avvio di nuove operazioni.
  FStopRequested := True;
  // Imposta il flag Terminated del thread se il thread esiste.
  if Assigned(FThread) then
  FThread.Terminate;
  // 2. Chiudi l'handle di scrittura di STDIN. Questo segnala al processo
  // che non arriveranno più comandi. È una buona pratica.
  if FStdInWrite <> 0 then
  begin
  CloseHandle(FStdInWrite);
  FStdInWrite := 0;
  end;
  // 3. Invia il comando 'exit' per far terminare il processo cmd.exe in modo pulito.
  // Questa chiamata potrebbe non essere più necessaria se chiudiamo le pipe,
  // ma la lasciamo per uno spegnimento "gentile".
  // NOTA: InternalWriteCommand fallirà se FStdInWrite è già chiuso, quindi
  // dovremmo spostare la chiusura di FStdInWrite dopo questa chiamata, o
  // rendere InternalWriteCommand a prova di handle chiuso.
  if FProcessHandle <> 0 then
  begin
  try
  InternalWriteCommand('exit');
  except end;
  // Attende la terminazione
  WaitForSingleObject(FProcessHandle, 5000);
  CloseHandle(FProcessHandle);
  FProcessHandle := 0;
  end;

  // 4. LA CHIAVE DELLA SOLUZIONE: Chiudi l'handle della pipe di lettura.

  // Questo sbloccherà immediatamente qualsiasi chiamata ReadFile nel ReaderThread.
  if FStdOutRead <> 0 then
  begin
  CloseHandle(FStdOutRead);
  FStdOutRead := 0;
  end;
  // 5. Ora che il thread di lettura è (o sarà presto) sbloccato, attendi la sua terminazione.
  // Metti un timeout per sicurezza, per non bloccare all'infinito in caso di problemi.
  if Assigned(FThread) then
  begin
  // TThread ha una proprietà Handle che è l'handle del thread a livello di OS
  if WaitForSingleObject(FThread.Handle, 5000) = WAIT_TIMEOUT then
  begin
  // Se il thread non termina entro 5 secondi, c'è un problema serio.
  if Assigned(FOnError) then
  FOnError('Timeout: il thread di lettura non è terminato correttamente.');

  // In questo caso estremo, potremmo considerare di terminare forzatamente il thread,
    // anche se è una pratica sconsigliata.
    // TerminateThread(FThread.Handle, 0);
  end;

  end;
  // 6. Infine, attendi la terminazione del processo e chiudi il suo handle.
  if FProcessHandle <> 0 then
  begin
  // Non c'è bisogno di un lungo wait qui, dovrebbe essere già terminato.
  WaitForSingleObject(FProcessHandle, 1000);
  // Termina forzatamente il processo se è ancora in esecuzione
  if WaitForSingleObject(FProcessHandle, 0) = WAIT_TIMEOUT then
    TerminateProcess(FProcessHandle, 0);

  CloseHandle(FProcessHandle);
  FProcessHandle := 0;

  end;
end;

function TShellRunnerPersistentCMD.IsRunning: Boolean;
begin
  // A process is running if its handle is valid and it hasn't signaled termination.
  // WaitForSingleObject with a 0ms timeout is the standard way to poll process status.
  Result := (FProcessHandle <> 0) and (WaitForSingleObject(FProcessHandle, 0) = WAIT_TIMEOUT);
end;

function TShellRunnerPersistentCMD.GetWorkingDirectory: string;
begin
  Result := FCurrentDir;
end;

procedure TShellRunnerPersistentCMD.SetWorkingDirectory(const Dir: string);
begin
  if not IsRunning then
  begin
    if Assigned(FOnError) then
      FOnError('Cannot set working directory, shell is not running.');
    Exit;
  end;

  if DirectoryExists(Dir) then
  begin
    // Use 'cd /d' to ensure the drive is changed as well.
    // No __CMD_END__ marker is needed, this is a fire-and-forget state change.
    InternalWriteCommand(Format('cd /d "%s"', [Dir]));
  end
  else
  begin
    if Assigned(FOnError) then
      FOnError(Format('Error setting working directory: Directory "%s" does not exist.', [Dir]));
  end;
end;

procedure TShellRunnerPersistentCMD.InternalWriteCommand(const Cmd: string);
var
  Line: AnsiString;
  BytesWritten: DWORD;
begin
  if (FStdInWrite = 0) or FStopRequested then
    Exit;

  // Attempt to track directory changes for GetWorkingDirectory
  TrackCurrentDirectory(Cmd);

  TMonitor.Enter(FLock);
  try
    // Append CRLF to execute the command
    Line := AnsiString(Cmd + #13#10);
    if not WriteFile(FStdInWrite, PAnsiChar(Line)^, Length(Line), BytesWritten, nil) then
    begin
      if Assigned(FOnError) then
        FOnError('Error writing to stdin: ' + SysErrorMessage(GetLastError));
    end;
  finally
    TMonitor.Exit(FLock);
  end;
end;

procedure TShellRunnerPersistentCMD.TrackCurrentDirectory(const Cmd: string);
var
  TrimmedCmd, NewDir: string;
begin
  // This is a best-effort attempt to track the current directory when a 'cd' command
  // is issued. It primarily works for absolute paths. Relative paths are not robustly handled.
  TrimmedCmd := Cmd.Trim;
  if TrimmedCmd.StartsWith('cd ', True) then
  begin
    NewDir := TrimmedCmd;
    if NewDir.StartsWith('cd /d ', True) then
      NewDir := NewDir.Substring(6).Trim(['"'])
    else if NewDir.StartsWith('cd ', True) then
      NewDir := NewDir.Substring(3).Trim(['"']);

    // If the path looks like an absolute path and exists, update our tracker.
    if TPath.IsPathRooted(NewDir) and DirectoryExists(NewDir) then
    begin
      FCurrentDir := TPath.GetFullPath(NewDir);
    end
    else
    begin
      // For relative paths, we try to resolve against the current directory
      var FullPath := TPath.GetFullPath(TPath.Combine(FCurrentDir, NewDir));
      if DirectoryExists(FullPath) then
        FCurrentDir := FullPath;
      end;
    // If the path is invalid, we do nothing. The shell will report an error,
    // and our tracked directory remains unchanged, which is correct.
  end;
end;


{ TReaderThread }

constructor TReaderThread.Create(const AOwner: TShellRunnerPersistentCMD);
begin
  inherited Create(False); // Create running
  FreeOnTerminate := True; // The thread will free itself on termination
  FOwner := AOwner;
end;

procedure TReaderThread.Execute;
var
  Buffer: array[0..1023] of AnsiChar;
  BytesRead: DWORD;
  LineBuffer: AnsiString;
  PosEOL: Integer;
  OutputLine: string;
  Marker: string;
begin
  NameThreadForDebugging('ShellReaderThread');
  LineBuffer := '';
  Marker := '__CMD_END__';

  try
    while not Terminated do
    begin
      BytesRead := 0;
      // ReadFile will block here until data is available or the pipe is broken
      if not ReadFile(FOwner.FStdOutRead, Buffer, SizeOf(Buffer) - 1, BytesRead, nil) then
      begin
        // ReadFile failed. This is the expected exit path when StopExecution closes the pipe.
        if GetLastError = ERROR_BROKEN_PIPE then
        begin
          if Assigned(FOwner.FOnError) then
            FOwner.FOnError('Pipe closed, reader thread terminating.');
        end
        else if not Terminated then // Don't log errors if we are already shutting down
        begin
          if Assigned(FOwner.FOnError) then
            FOwner.FOnError('ReadFile error in reader thread: ' + SysErrorMessage(GetLastError));
        end;
        Break; // Exit the loop
      end;

      if BytesRead = 0 then
        Break; // End of file on the pipe, process has likely exited.

      Buffer[BytesRead] := #0;
      LineBuffer := LineBuffer + AnsiString(Buffer);

      // Process the buffer line by line
      repeat
        PosEOL := Pos(#13#10, LineBuffer);
        if PosEOL > 0 then
        begin
          OutputLine := string(Copy(LineBuffer, 1, PosEOL - 1));
          Delete(LineBuffer, 1, PosEOL + 1); // Remove the line and its CRLF

          if Pos(Marker, OutputLine) = 1 then
          begin
            if Assigned(FOwner.FOnExit) then
              FOwner.FOnExit(-1); // Signal command finished
          end
          else if Assigned(FOwner.FOnOutput) then
          begin
            FOwner.FOnOutput(OutputLine);
          end;
        end;
      until PosEOL = 0;
    end;
  except
    on E: Exception do
      if Assigned(FOwner.FOnError) then
        FOwner.FOnError('Exception in reader thread: ' + E.Message);
  end;
end;

end.
