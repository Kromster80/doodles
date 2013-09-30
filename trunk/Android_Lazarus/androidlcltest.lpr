library androidlcltest;

uses cmem,gles,egl,ctypes,
native_activity,native_window,
     looper, input,
     android_native_app_glue,
     log;

type
  Psaved_state = ^Tsaved_state;
  Tsaved_state = packed record
    angle : cfloat;
    x : cint32;
    y : cint32;
  end;

  Pengine = ^Tengine;
  Tengine = packed record
    app : Pandroid_app;
    animating : cint;
    display : EGLDisplay;
    surface : EGLSurface;
    context : EGLContext;
    width : cint32;
    height : cint32;
    state : Tsaved_state;
  end;

const
 attribs: array[0..8] of EGLint = (
           EGL_SURFACE_TYPE, EGL_WINDOW_BIT,
           EGL_BLUE_SIZE, 8,
           EGL_GREEN_SIZE, 8,
           EGL_RED_SIZE, 8,
           EGL_NONE);

function engine_init_display(engine: Pengine): cint;
var w, h, dummy, format,numConfigs: EGLint;
    config: EGLConfig;
    surface: EGLSurface;
    context: EGLContext;
    display: Pointer;
begin
   // initialize OpenGL ES and EGL

   (*
    * Here specify the attributes of the desired configuration.
    * Below, we select an EGLConfig with at least 8 bits per color
    * component compatible with on-screen windows
     *)

   display := eglGetDisplay(EGL_DEFAULT_DISPLAY);

   eglInitialize(display, nil,nil);

(* Here, the application chooses the configuration it desires. In this
    * sample, we have a very simplified selection process, where we pick
    * the first EGLConfig that matches our criteria  *)

   eglChooseConfig(display, attribs, @config, 1, @numConfigs);

(* EGL_NATIVE_VISUAL_ID is an attribute of the EGLConfig that is
    * guaranteed to be accepted by ANativeWindow_setBuffersGeometry().
    * As soon as we picked a EGLConfig, we can safely reconfigure the
    * ANativeWindow buffers to match, using EGL_NATIVE_VISUAL_ID.  *)

   eglGetConfigAttrib(display, config, EGL_NATIVE_VISUAL_ID, @format);

   ANativeWindow_setBuffersGeometry(engine^.app^.window, 0, 0, format);

   surface := eglCreateWindowSurface(display, config, engine^.app^.window, nil);
   context := eglCreateContext(display, config, nil, nil);

   if eglMakeCurrent(display, surface, surface, context) = EGL_FALSE then
   begin
       LOGW('Unable to eglMakeCurrent');
       exit(-1);
   end;

   eglQuerySurface(display, surface, EGL_WIDTH, @w);
   eglQuerySurface(display, surface, EGL_HEIGHT, @h);

   engine^.display := display;
   engine^.context := context;
   engine^.surface := surface;
   engine^.width := w;
   engine^.height := h;
   engine^.state.angle := 0;

   // Initialize GL state.
   glHint(GL_PERSPECTIVE_CORRECTION_HINT, GL_FASTEST);
   glEnable(GL_CULL_FACE);
   glShadeModel(GL_SMOOTH);
   glDisable(GL_DEPTH_TEST);

   result := 0;
end;

procedure engine_draw_frame(engine: Pengine);
begin
   if engine^.display = nil then
      exit;

   // Just fill the screen with a color.
   glClearColor(engine^.state.x/engine^.width, engine^.state.angle, engine^.state.y/engine^.height, 1);
   glClear(GL_COLOR_BUFFER_BIT);

   eglSwapBuffers(engine^.display, engine^.surface);
end;


procedure engine_term_display(engine: Pengine);
begin
   if (engine^.display <> EGL_NO_DISPLAY) then
   begin
       eglMakeCurrent(engine^.display, EGL_NO_SURFACE, EGL_NO_SURFACE, EGL_NO_CONTEXT);
       if (engine^.context <> EGL_NO_CONTEXT) then
           eglDestroyContext(engine^.display, engine^.context);
       if (engine^.surface <> EGL_NO_SURFACE) then
           eglDestroySurface(engine^.display, engine^.surface);
       eglTerminate(engine^.display);
   end;

   engine^.animating := 0;
   engine^.display := EGL_NO_DISPLAY;
   engine^.context := EGL_NO_CONTEXT;
   engine^.surface := EGL_NO_SURFACE;
end;

procedure engine_handle_cmd(app: Pandroid_app; cmd: cint32); cdecl;
var engine: Pengine;
begin
   engine := Pengine(app^.userData);
   case cmd of
      APP_CMD_SAVE_STATE:
         begin
            // The system has asked us to save our current state.  Do so.
            engine^.app^.savedState := malloc(sizeof(Tsaved_state));
            Psaved_state(engine^.app^.savedState)^ := engine^.state;
            engine^.app^.savedStateSize := sizeof(Tsaved_state);
         end;
      APP_CMD_INIT_WINDOW:
         begin
            // The window is being shown, get it ready.
            if (engine^.app^.window <> Nil) then
            begin
               LOGW('Initializing display');
               engine_init_display(engine);
               engine_draw_frame(engine);
            end;
         end;
      APP_CMD_TERM_WINDOW:
         begin
            // The window is being hidden or closed, clean it up.
            engine_term_display(engine);
         end;
      APP_CMD_GAINED_FOCUS:
         begin
            // When our app gains focus, we start monitoring the accelerometer.
            {if (engine^.accelerometerSensor <> Nil) then
            begin
               ASensorEventQueue_enableSensor(engine^.sensorEventQueue, engine^.accelerometerSensor);
               // We'd like to get 60 events per second (in us).
               ASensorEventQueue_setEventRate(engine^.sensorEventQueue, engine^.accelerometerSensor, (1000L/60)*1000);
            end;}
         end;
      APP_CMD_LOST_FOCUS:
         begin
            // When our app loses focus, we stop monitoring the accelerometer.
            // This is to avoid consuming battery while not being used.
            {if engine^.accelerometerSensor <> NULL then
               ASensorEventQueue_disableSensor(engine^.sensorEventQueue, engine^.accelerometerSensor);}
            // Also stop animating.
            engine^.animating := 0;
            engine_draw_frame(engine);
         end;
   end;
end;

function engine_handle_input(app: Pandroid_app; event: PAInputEvent): cint32; cdecl;
var engine: Pengine;
begin
   engine := Pengine(app^.userData);
   if AInputEvent_getType(event) = AINPUT_EVENT_TYPE_MOTION then
   begin
      engine^.animating := 1;
      {engine^.state.x := AMotionEvent_getX(event, 0);
      engine^.state.y := AMotionEvent_getY(event, 0);}
      result := 1;
   end
   else
      result := 0;
end;

procedure android_main(state: Pandroid_app); cdecl; export;
var engine: Tengine;
    ident,events: cint;
    source: Pandroid_poll_source;
    val: cint;
begin
   // Make sure glue isn't stripped.
   app_dummy();
   LOGW('Android main!');

   FillChar(engine, sizeof(Tengine), 0);
   LOGW('Android main 2!');

   state^.userData := @engine;
   state^.onAppCmd := @engine_handle_cmd;
   state^.onInputEvent := @engine_handle_input;
   engine.app := state;
   LOGW('Android main 3!');

   if state^.savedState <> nil then
      // We are starting with a previous saved state; restore from it.
      engine.state := Psaved_state(state^.savedState)^;

   LOGW('Entering loop');
   // loop waiting for stuff to do.

   while true do
   begin// Read all pending events.
      // If not animating, we will block forever waiting for events.
      // If animating, we loop until all events are read, then continue
      // to draw the next frame of animation.

      if engine.animating<>0 then
         val := 0
      else
         val := -1;
      ident := ALooper_pollAll(val, nil, @events,@source);
      while (ident >= 0) do
      begin
         // Process this event.
         if (source <> nil) then
            source^.process(state, source);

         // If a sensor has data, process it now.
         if (ident = LOOPER_ID_USER) then
         begin
            {if (engine.accelerometerSensor != nil) then
            begin
               ASensorEvent event;
               while (ASensorEventQueue_getEvents(engine.sensorEventQueue, &event, 1) > 0) do
               begin
                  LOGI("accelerometer: x=%f y=%f z=%f",
                          [event.acceleration.x, event.acceleration.y,
                          event.acceleration.z]);
               end;
            end;}
         end;

         // Check if we are exiting.
         if (state^.destroyRequested <> 0) then
         begin
            LOGW('Destroy requested');
            engine_term_display(@engine);
            exit;
         end;

         if engine.animating<>0 then
            val := 0
         else
            val := -1;
         ident := ALooper_pollAll(val, nil, @events,@source);
      end;

      if engine.animating <> 0 then
      begin
         // Done with events; draw next animation frame.
         engine.state.angle := engine.state.angle + 0.01;
         if (engine.state.angle > 1) then
            engine.state.angle := 0;
      end;

      // Drawing is throttled to the screen update rate, so there
      // is no need to do timing here.
      engine_draw_frame(@engine);
   end;
end;


exports //android_main name 'android_main',
        ANativeActivity_onCreate name 'ANativeActivity_onCreate';

end.
