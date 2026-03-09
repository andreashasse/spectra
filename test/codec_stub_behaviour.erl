-module(codec_stub_behaviour).

%% A minimal behaviour used only in tests, so that codec_multi_behaviour_module
%% can declare spectra_codec as its *second* -behaviour attribute without
%% triggering missing-callback warnings for a real OTP behaviour.
-callback stub() -> ok.
-optional_callbacks([stub/0]).
