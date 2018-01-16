library AudioMixerPlugin;

{$mode objfpc}{$H+}
{$CODEPAGE UTF-8}

uses
  AviUtl,
  CircularBuffer,
  Lag,
  RbjEQFilter,
  MDADynamics,
  ChannelStrip,
  AudioMixer,
  AudioMixerMain,
  OnePole,
  FixedCircularBuffer,
  Reverb,
  MultiTapDelay,
  Vibrato,
  AllPass,
  AuxChannelStrip;

exports
  GetFilterTableList;

begin
  SetMultiByteConversionCodePage(CP_UTF8);
  Randomize();
end.
