#define MyAppName "Humbug"
#define MyAppVersion "41"
#define MyAppPublisher "m6r.ai"
#define MyAppURL "https://github.com/m6r-ai/humbug"
#define MyAppExeName "Humbug.exe"

[Setup]
AppId={{A1B2C3D4-E5F6-7890-ABCD-EF1234567890}
AppName={#MyAppName}
AppVersion={#MyAppVersion}
AppPublisher={#MyAppPublisher}
AppPublisherURL={#MyAppURL}
AppSupportURL={#MyAppURL}
AppUpdatesURL={#MyAppURL}
DefaultDirName={autopf}\{#MyAppName}
DefaultGroupName={#MyAppName}
AllowNoIcons=yes
OutputDir=dist
OutputBaseFilename=Humbug-Setup
SetupIconFile=icons\Humbug.ico
Compression=lzma
SolidCompression=yes
WizardStyle=modern
PrivilegesRequired=lowest
ArchitecturesInstallIn64BitMode=x64compatible
UninstallDisplayIcon={app}\{#MyAppExeName}
CloseApplications=no
Uninstallable=yes
CreateUninstallRegKey=yes

[Languages]
Name: "english"; MessagesFile: "compiler:Default.isl"

[Tasks]
Name: "desktopicon"; Description: "{cm:CreateDesktopIcon}"; GroupDescription: "{cm:AdditionalIcons}"; Flags: unchecked

[Files]
Source: "dist\{#MyAppExeName}"; DestDir: "{app}"; Flags: ignoreversion

[Icons]
Name: "{group}\{#MyAppName}"; Filename: "{app}\{#MyAppExeName}"
Name: "{group}\{cm:UninstallProgram,{#MyAppName}}"; Filename: "{uninstallexe}"
Name: "{autodesktop}\{#MyAppName}"; Filename: "{app}\{#MyAppExeName}"; Tasks: desktopicon

[UninstallDelete]
Type: filesandordirs; Name: "{app}"

[Code]
var
  DeleteUserData: Boolean;

function IsHumbugRunning(): Boolean;
var
  WbemLocator, WbemServices, WbemObjectSet: Variant;
begin
  Result := False;
  try
    WbemLocator := CreateOleObject('WbemScripting.SWbemLocator');
    WbemServices := WbemLocator.ConnectServer('.', 'root\CIMV2');
    WbemObjectSet := WbemServices.ExecQuery('SELECT * FROM Win32_Process WHERE Name = "Humbug.exe"');
    Result := WbemObjectSet.Count > 0;
  except
    Result := False;
  end;
end;

function InitializeUninstall(): Boolean;
begin
  Result := True;

  if IsHumbugRunning() then
  begin
    MsgBox(
      'Humbug is currently running.' + #13#10 +
      'Please close the application before uninstalling.',
      mbError, MB_OK
    );
    Result := False;
    Exit;
  end;

  if MsgBox(
    'Are you sure you want to uninstall Humbug?',
    mbConfirmation, MB_YESNO
  ) = IDNO then
  begin
    Result := False;
    Exit;
  end;

  DeleteUserData := MsgBox(
    'Do you want to also remove all Humbug user data?' + #13#10 +
    '(API keys, settings, and conversations)',
    mbConfirmation, MB_YESNO
  ) = IDYES;
end;

procedure CurUninstallStepChanged(CurUninstallStep: TUninstallStep);
var
  AppDataPath: String;
begin
  if CurUninstallStep = usUninstall then
  begin
    if DeleteUserData then
    begin
      AppDataPath := ExpandConstant('{%USERPROFILE}\.humbug');
      if DirExists(AppDataPath) then
        DelTree(AppDataPath, True, True, True);
    end;
  end;
end;

[Run]
Filename: "{app}\{#MyAppExeName}"; Description: "{cm:LaunchProgram,{#StringChange(MyAppName, '&', '&&')}}"; Flags: nowait postinstall skipifsilent
