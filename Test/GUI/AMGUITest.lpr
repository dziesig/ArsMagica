program AMGUITest;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}{$IFDEF UseCThreads}
  cthreads,
  {$ENDIF}{$ENDIF}
  Interfaces, // this includes the LCL widgetset
  Forms, page1, page2, page3, amguitestformUnit, amaboutformunit, 
ammainformbase;

{$R *.res}

begin
  Application.Scaled:=True;
  RequireDerivedFormResource:=True;
  Application.Initialize;
  Application.CreateForm(TAMGuiTestForm, AMGuiTestForm);
  Application.CreateForm(TPage1Form, Page1Form);
  Application.CreateForm(TPage2Form, Page2Form);
  Application.CreateForm(TPage3Form, Page3Form);
  Application.CreateForm(TAMAboutForm, AMAboutForm);
  Application.CreateForm(TAMMainFormBase, AMMainFormBase);
  Application.Run;
end.

