unit antiHacking.Security.Config;

interface

{$IFDEF DEBUG}
  {$DEFINE SECURITY_RELAXED}
{$ENDIF}

const
  EXPECTED_EXE_SHA256_HEX = 'REPLACE_IN_CI';

implementation

end.



