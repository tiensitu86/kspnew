unit indylaz;
interface

uses
  IdAbout,
  IdAboutVCL,
  IdAntiFreeze,
  IdCoreDsnRegister,
  IdDsnBaseCmpEdt,
  IdDsnCoreResourceStrings,
  IdDsnPropEdBinding,
  IdDsnPropEdBindingVCL,
  IdDsnRegister,
  IdDsnResourceStrings,
  IdDsnSASLListEditor,
  IdDsnSASLListEditorForm,
  IdDsnSASLListEditorFormVCL,
  IdRegister,
  IdRegisterCore;

implementation

procedure Register;
begin
  RegisterUnit('IdCoreDsnRegister', @IdCoreDsnRegister.Register);
  RegisterUnit('IdDsnRegister', @IdDsnRegister.Register);
  RegisterUnit('IdRegister', @IdRegister.Register);
  RegisterUnit('IdRegisterCore', @IdRegisterCore.Register);
end;

initialization
  RegisterPackage('indylaz', @Register);
end.
