MODULE mand01;

IMPORT	x11, InOut, Arguments, NumConv, RealConv, MathLib;

TYPE	Complex	= RECORD
		    real, imag	: REAL
		  END;

VAR	z, z0, c		: Complex;
	startR, startI, step	: REAL;
	count, n		: CARDINAL;
	Mx, My, task		: INTEGER;
	store			: Arguments.ArgTable;
	colours			: ARRAY [0..19] OF CARDINAL;


PROCEDURE Csq (VAR  z : Complex);

VAR	res	: Complex;

BEGIN
  res.real := z.real * z.real - z.imag * z.imag;
  res.imag := 2.0 * z.real * z.imag;
  z := res
END Csq;


PROCEDURE Cadd (VAR z : Complex;  y : Complex);

VAR	res	: Complex;

BEGIN
  res.real := y.real + z.real;
  res.imag := y.imag + z.imag;
  z := res
END Cadd;


PROCEDURE Csize (VAR z : Complex) : INTEGER;

VAR	len	: INTEGER;

BEGIN
  len := MathLib.entier (z.real * z.real + z.imag * z.imag);
  RETURN len
END Csize;


PROCEDURE Init;

VAR	args	: SHORTCARD;
	ok	: BOOLEAN;

BEGIN
  Arguments.GetArgs (args, store);
  IF  args < 4  THEN
    InOut.WriteString ("Mandel X0 Y0 count step");	InOut.WriteLn;
    startR := -0.71;
    startI := -0.36;
    count := 100;
    step := 40000.0
  ELSE
    startR := RealConv.Str2Real (store^ [1]^, ok);
    startI := RealConv.Str2Real (store^ [2]^, ok);
    NumConv.Str2Num (count, 10, store^ [3]^, ok);
    step := RealConv.Str2Real (store^ [4]^, ok);
    InOut.WriteLn
  END;
  z0.real := 0.0;
  z0.imag := 0.0;
  colours [0] := 0FF0000H;
  colours [1] := 0FF4F00H;
  colours [2] := 0FF9F00H;
  colours [3] := 0FF9F4FH;
  colours [4] := 0FF9F9FH;
  colours [5] := 0FFFF00H;
  colours [6] := 0FFFF4FH;
  colours [7] := 0FFFF9FH;
  colours [8] := 000FFFFH;
  colours [9] := 00000FFH
END Init;


PROCEDURE build;

VAR	X0, Y0,
	x, y, color	: INTEGER;

BEGIN
  FOR  X0 := 0 TO 799  DO
    x := X0 - 400;
    c.real := startR + MathLib.real (x) / step;
    FOR  Y0 := 499 TO 0 BY -1  DO
      y := Y0 - 250;
      c.imag := startI + MathLib.real (y) / step;
      n := 0;
      z := z0;
      REPEAT
        INC (n);
	Csq (z);
	Cadd (z, c)
      UNTIL  (Csize (z) > 4) OR (n > count);
      IF  n < count  THEN
(*	color := 10 * n DIV count;
	x11.setFgC (colours [color]);	*)
	x11.setFgC (99 * n);
	x11.Plot (X0, Y0)
      END
    END
  END
END build;


BEGIN
  Init;
  x11.setTitle ("Mandelbrot with Mocka");
  x11.Xinit (800, 500);
  x11.SetFont ("10x20");
  task := x11.NextEvent ();
  build;
  LOOP
    task := x11.NextEvent ();
    IF  task = -1000  THEN
      build
    ELSIF  task < 0  THEN
      task := -task;
      IF  task = ORD ('q')  THEN  EXIT  END
    ELSIF  task > 0  THEN	(* Mouse click!	*)
      Mx := task DIV 10000;
      My := task MOD 10000;
    END
  END;
  x11.Xclose ()
END mand01.
