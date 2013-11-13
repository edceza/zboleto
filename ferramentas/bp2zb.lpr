program bp2zb;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}{$IFDEF UseCThreads}
  cthreads,
  {$ENDIF}{$ENDIF}
  Interfaces, Forms, frmprinc;

begin
  RequireDerivedFormResource := True;
  Application.Initialize;
  Application.CreateForm(TfrPrinc, frPrinc);
  Application.Run;
end.

