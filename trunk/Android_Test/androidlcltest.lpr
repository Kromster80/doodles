library androidlcltest;

{$mode objfpc}{$H+}

uses
  customdrawnint,
  Interfaces,
  Forms,
  mainform, secondform, customdrawn_android, customdrawndrawers,
  egl, native_window, sysutils;

exports
  Java_com_pascal_lclproject_LCLActivity_LCLOnTouch name 'Java_com_pascal_lcltest_LCLActivity_LCLOnTouch',
  Java_com_pascal_lclproject_LCLActivity_LCLDrawToBitmap name 'Java_com_pascal_lcltest_LCLActivity_LCLDrawToBitmap',
  Java_com_pascal_lclproject_LCLActivity_LCLOnCreate name 'Java_com_pascal_lcltest_LCLActivity_LCLOnCreate',
  Java_com_pascal_lclproject_LCLActivity_LCLOnMessageBoxFinished name 'Java_com_pascal_lcltest_LCLActivity_LCLOnMessageBoxFinished',
  Java_com_pascal_lclproject_LCLActivity_LCLOnKey name 'Java_com_pascal_lcltest_LCLActivity_LCLOnKey',
  Java_com_pascal_lclproject_LCLActivity_LCLOnTimer name 'Java_com_pascal_lcltest_LCLActivity_LCLOnTimer',
  Java_com_pascal_lclproject_LCLActivity_LCLOnConfigurationChanged name 'Java_com_pascal_lcltest_LCLActivity_LCLOnConfigurationChanged',
  Java_com_pascal_lclproject_LCLActivity_LCLOnSensorChanged name 'Java_com_pascal_lcltest_LCLActivity_LCLOnSensorChanged',
  Java_com_pascal_lclproject_LCLActivity_LCLOnMenuAction name 'Java_com_pascal_lcltest_LCLActivity_LCLOnMenuAction',
  JNI_OnLoad name 'JNI_OnLoad',
  JNI_OnUnload name 'JNI_OnUnload';

procedure MyActivityOnCreate;
const
 attribs: array[0..8] of EGLint = (
           EGL_SURFACE_TYPE, EGL_WINDOW_BIT,
           EGL_BLUE_SIZE, 8,
           EGL_GREEN_SIZE, 8,
           EGL_RED_SIZE, 8,
           EGL_NONE);
var
  format, numConfigs: EGLint;
  config: EGLConfig;
  display: Pointer;
  surface: EGLSurface;
  context: EGLContext;
begin
  display := eglGetDisplay(EGL_DEFAULT_DISPLAY);
  eglInitialize(display, nil, nil);
  eglChooseConfig(display, attribs, @config, 1, @numConfigs);
  eglGetConfigAttrib(display, config, EGL_NATIVE_VISUAL_ID, @format);
  //ANativeWindow_setBuffersGeometry(engine^.app^.window, 0, 0, format);
  //surface := eglCreateWindowSurface(display, config, engine^.app^.window, nil);
  context := eglCreateContext(display, config, nil, nil);


  DefaultStyle := dsAndroid;
  Application.Initialize;
  Application.CreateForm(TForm1, Form1);
  Application.CreateForm(TForm2, Form2);

  Form1.Button1.Caption := IntToStr(numConfigs);

  Application.Run;
end;

begin
  CDWidgetset.ActivityOnCreate := @MyActivityOnCreate;
end.

