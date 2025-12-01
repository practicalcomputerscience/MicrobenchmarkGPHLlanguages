% random_streams_for_perf_stats.m
%
% A program for the Mercury logic programming system: https://github.com/Mercury-Language
%
% 2025-11-02/03, 2025-12-01: have small hex letters a...f
%
% predicate =~ statement
%
%
% build on Ubuntu 24 LTS: $ mmc random_streams_for_perf_stats.m
%
% run on Ubuntu 24 LTS:   $ ./random_streams_for_perf_stats
%                         $ sudo perf stat -r 20 ./random_streams_for_perf_stats
%
%
% > mmc --version
% Mercury Compiler, version rotd-2025-11-01
% Copyright (C) 1993-2012 The University of Melbourne
% Copyright (C) 2013-2025 The Mercury team
% >
%
%-----------------------------------------------------------------------------%

:- module random_streams_for_perf_stats.  % declarations and clauses end with a full stop
:- interface.                             % things to be exported
:- import_module io.                      % things to be imported


:- pred main(io::di, io::uo) is det.  % declare a predicate called main, which is
                                      % needed for the compiler as the starting point:
                                      %   arg.#1: di for destructive input
                                      %   arg.#2: uo for unique output
                                      % is det = deterministic predicate, a must for I/O op's:
                                      %   with same input there will always be same output
                                      % Arguments may be either input or output!
                                      % io::di is a short form for:
                                      %   :- pred main(io, io).
                                      %   :- mode main(di, uo).

:- implementation.

% declarations at the start of the implementation section:
:- import_module uint16, uint64, int, string, list.  % list for io.format()
:- import_module char.
:- import_module random, random.sfc16.               % sfc = small fast counting generator
:- import_module stream.
:- import_module stream.string_writer.
:- import_module string.builder.


%-----------------------------------------------------------------------------%
%
% user defined functions

% this solution is completely and 1:1 based on MS Bing AI prompt:
%   "Mercury language get system time in milliseconds"
% Foreign import of a C function to get time in milliseconds
:- pred get_time_ms(int::out) is det.
:- pragma foreign_proc("C",
    get_time_ms(Ms::out),
    [will_not_call_mercury, promise_pure, thread_safe],
"
    struct timeval tv;
    gettimeofday(&tv, NULL);
    Ms = (int)((tv.tv_sec * 1000LL) + (tv.tv_usec / 1000));
").

:- pragma foreign_decl("C", "
    #include <sys/time.h>
    #include <stdint.h>
").


:- pred get_int_random(int::in, int::in, int::out, R::in, R::out) is det <= random(R).
                                                                       % <= is for a type class constraint
get_int_random(Start, Range, IntRandNbr, !R) :-
  random.uniform_int_in_range(Start, Range, IntRandNbr, !R).


% this padding is not working (but the conversion to a string):
%   :- func integer_to_bin_string(int) = string.
%   integer_to_bin_string(N) = PaddedBinString :-
%     (StrRaw = string.int_to_base_string(N, 2),
%      StrPadLen = 16 - string.length(StrRaw),
%      ( if StrPadLen > 0 then
%         PaddedBinString = string.pad_left(StrRaw, '0', StrPadLen)
%      else
%         PaddedBinString = StrRaw
%      )
%     ).
%     % https://github.com/Mercury-Language/mercury/blob/fca4505501852e5feda0734ff6c5ec6ac02bc638/tests/hard_coded/bitmap_test.m#L511

% MS Bing AI: this solution works!
:- func pad_left(string, int) = string.
pad_left(S, Width) = Result :-
  Len = string.length(S),
  ( if Len >= Width then
      Result = S  % No padding needed
  else
      PadCount = Width - Len,
      PadStr = string.from_char_list(list.duplicate(PadCount, '0')),
      Result = PadStr ++ S
  ).


% MS Bing AI based solution:
:- pred write_to_file(string::in, string::in, string::in, io::di, io::uo) is det.
write_to_file(FileName, Content, FileType, !IO) :-
  io.open_output(FileName, OpenResult, !IO),
  (
    OpenResult = ok(Stream),
    io.write_string(Stream, Content, !IO),
    io.close_output(Stream, !IO),

    ( if FileType = "bit" then
       io.format("\nBit stream has been written to disk under name:  %s", [s(FileName)], !IO)
    else
       io.format("\nByte stream has been written to disk under name: %s", [s(FileName)], !IO)
    )
  ;
    OpenResult = error(Error),
    io.format("\ncould not write to file: %s -- %s",
              [s(FileName), s(io.error_message(Error))], !IO)
  ).


%**********************  recursive master loop  ****************************%
%
% values are immutable in this purely declarative language;
% then build all output lists recursively:
%
:- pred masterloop(int::in, int::in, list(int)::out, list(string)::out, list(string)::out) is det.
masterloop(Length, Seed, X, BitsX, BitsHex) :-
  ( if Length =< 0 then
     X = [],  % return empty list
     BitsX = [],
     BitsHex = []
  else
     NewSeed = (17364 * Seed + 0) `mod` 65521,  % there are no global constants in Mercury!

     % recursion:
     masterloop(Length - 1, NewSeed, XPrev, BitsXPrev, BitsHexPrev),
     % what a trick with the introduction of XPrev etc.! MS Bing AI helped me out here.

     % convert random integer number into its binary string representation:
     NewBitsX0 = string.int_to_base_string(NewSeed, 2),
     NewBitsX  = pad_left(NewBitsX0, 16),

     NewBitsHex0a = string.int_to_base_string(NewSeed, 16),  % hexadecimal string representation, but in capital letters A..F
     NewBitsHex0  = string.to_lower(NewBitsHex0a),  % hexadecimal string representation in correct small letters a..f
     NewBitsHex   = pad_left(NewBitsHex0, 4),                

     % build lists and strings:
     X       = [NewSeed | XPrev],  % [|] is the non-empty list constructor, pronounced "cons"
     BitsX   = [NewBitsX | BitsXPrev],
     BitsHex = [NewBitsHex | BitsHexPrev]
  ).
% end of masterloop
%***************************************************************************%

% end of user defined functions
%
%-----------------------------------------------------------------------------%


main(!IO) :-

  End = 62500,  % 62500 for exactly 1M binary digits; user defined Variables start with a Capital letter!
                  % Otherwise, these names would be symbol names.
  % End = 10,  % for testing

  M = 65521,  % = 2^16 - 15

  FileBitsX   = "random_bitstring.bin",
  FileBitsHex = "random_bitstring.byte",

  get_time_ms(Ms),
  % io.format("Current time: %d ms since epoch\n", [i(Ms)], !IO).  % for testing
  Time0 = uint64.cast_from_int(Ms),      % type conversion into uint64
  R0 = sfc16.seed(Time0),                % initialize a 16-bit SFC generator with a time based seed
  get_int_random(1, M, X0, R0, _),       % _: discard R::out; X0 is a dynamic random seed for the master loop
  % io.format("X0 = %d\n", [i(X0)], !IO),  % for testing


  io.write_string("\ngenerating a random bit stream...", !IO),

  masterloop(End, X0, _, BitsX, BitsHex),  % 2 x input, 3 x output

  % Str = string(X),  % for testing
  % io.format("\nX = %s\n", [s(Str)], !IO),  % for testing

  Builder0  = string.builder.init,
  string.builder.append_strings(BitsX, Builder0, Builder1),
  BitsXStr  = string.builder.to_string(Builder1),
  % io.format("\nBitsXStr = %s\n", [s(BitsXStr)], !IO),  % for testing

  Builder2  = string.builder.init,
  string.builder.append_strings(BitsHex, Builder2, Builder3),
  BitsHexStr = string.builder.to_string(Builder3),
  % io.format("BitsHexStr = %s\n", [s(BitsHexStr)], !IO),  % for testing

  write_to_file(FileBitsX, BitsXStr, "bit", !IO),
  write_to_file(FileBitsHex, BitsHexStr, "byte", !IO),

  io.write_string("\n", !IO).

% end of random_streams_for_perf_stats.m
