/* random_streams_for_perf_stats.oz

2025-12-06/07/08

build program in Windows 11 cmd: >ozc -c random_streams_for_perf_stats.oz
run program in Windows 11 cmd:   >ozengine random_streams_for_perf_stats.ozf


>ozc -v
Mozart Compiler 1.4.0 (20080703) playing Oz 3
...
>

*/

functor
import
  System
  Application
  OS  % rand + srand
  Open(file)

define

    %-----------------------------------------------------------------------------%
    %
    % user defined functions

    % helper function: recursively make a list of base F integer divisions:
    fun {DivList N F}
        if N =< 0 then
            nil
        else
            N mod F | {DivList (N div F) F}
        end
    end

    % helper function: recursively pad a string to target length Len:
    fun {Pad_string_to Raw_str Len}
        local
            % recursively build a padding string of '0' chars:
            fun {Padding Len}
                if Len =< 0 then
                    nil
                else
                    48 | {Padding (Len - 1)}
                end
            end

            Fix_len
        in
            Fix_len = Len - {Length Raw_str}

            if Fix_len >=0 then
                {Append Raw_str {Padding Fix_len}}
            else
                Raw_str
            end
        end
    end


    fun {Integer_to_bin_string N}
    % elegant idea from: https://stackoverflow.com/questions/73081826/mozart-programming-integer-to-hexadecimal-conversion-in-oz
        local
            Bin_str1 Bin_str2 Bin_str3
        in
            Bin_str1 = {FoldR {DivList N 2} fun {$ N Nr}
                                              if N == 0 then
                                                  48 | Nr  % '0'
                                              else
                                                  49 | Nr  % '1'
                                              end
                                            end
                                            nil}

            Bin_str2 = {Pad_string_to Bin_str1 16}

            Bin_str3 = {List.reverse Bin_str2}

            {String.toAtom Bin_str3}
        end
    end  % of Integer_to_bin_string


    fun {Integer_to_hex_string N}
        local
            Bin_str1 Bin_str2 Bin_str3
        in
            Bin_str1 = {FoldR {DivList N 16} fun {$ N Nr}
                                                 if N == 10 then &A | Nr
                                                 elseif N == 11 then &B | Nr
                                                 elseif N == 12 then &C | Nr
                                                 elseif N == 13 then &D | Nr
                                                 elseif N == 14 then &E | Nr
                                                 elseif N == 15 then &F | Nr
                                                 else
                                                     {Int.toString N}.1 | Nr
                                                 end
                                             end
                                             nil}

            Bin_str2 = {Pad_string_to Bin_str1 4}

            Bin_str3 = {List.reverse Bin_str2}

            {String.toAtom Bin_str3}
        end
    end  % of Integer_to_hex_string


    proc {Write_to_file FileName Atom_array FileType}
        % see from: https://github.com/mozart/mozart2/blob/83c83da2f670fbd1d08d4145eca3d88f1687582c/lib/tools/panel/panel/top.oz#L670
        try
            F = {New Open.file init(name: FileName flags: [write create])}
        in
            local AtomStr
            in
                if FileType == 'bit' then
                    AtomStr = {NewCell '0000000000000000'}
                else
                    AtomStr = {NewCell '0000'}
                end

                % {System.showInfo "\n======"}  % for testing
                for I in 1..End do
                    AtomStr := {Array.get Atom_array I}
                    % {System.showInfo "\nAtomStr = "#{Value.toVirtualString @AtomStr 0 0}}  % for testing: AtomStr = '0010101111001011'

                    {F write(vs:@AtomStr)}  % write individual string atoms to the file
                end

                {F close}

                if FileType == 'bit' then
                    {System.showInfo "Bit stream has been written to disk under name:  "#{Value.toVirtualString FileName 0 0}}
                else
                    {System.showInfo "Byte stream has been written to disk under name: "#{Value.toVirtualString FileName 0 0}}
                end
            end

        catch system(os(_ _ _ S) ...) then
            {System.showInfo "could not write to file: "#{Value.toVirtualString FileName 0 0}#' -- '#S}
            % could not write to file: 'random_bitstring.bin' -- Input/output error
        end
    end  % of write_to_file

    % end of user defined functions
    %
    %-----------------------------------------------------------------------------%


    End = 62500  % 62500 for exactly 1M binary digits
    % End = 10  % for testing

    M = 65521  % = 2^16 - 15
    A = 17364
    C = 0
    % from: https://statmath.wu.ac.at/software/src/prng-3.0.2/doc/prng.html/Table_LCG.html

    File_bits_x   = 'random_bitstring.bin'
    File_bits_hex = 'random_bitstring.byte'

    X        = {Array.new 1 End 0}  % initializing all elements to 0
    Bits_x   = {Array.new 1 End '0000000000000000'}
    Bits_hex = {Array.new 1 End '0000'}

    % http://mozart2.org/mozart-v1/doc-1.4.0/system/node56.html#label721
    {OS.srand 0}  % https://linux.die.net/man/3/srand: modulo 32768
    % not great but much easier than a FFI to a C solution with Oz
    % I'm also not willing to add more lines of code here
    X0 = {OS.rand}
    % {System.showInfo "X0 = "#{Value.toVirtualString X0 0 0}}  % for testing
    % Bits_x_str0 = {Integer_to_bin_string X0}  % for testing: Bits_x_str0 = '111110100100011'
    % {System.showInfo "Bits_x_str0 = "#{Value.toVirtualString Bits_x_str0 0 0}}  % for testing

    % first array values: build X, Bits_x, Bits_hex in sync:
    {Array.put X        1 X0}
    {Array.put Bits_x   1 {Integer_to_bin_string X0}}
    {Array.put Bits_hex 1 {Integer_to_hex_string X0}}


    {System.showInfo "\ngenerating a random bit stream..."}

    % imperative solution:
    proc {Masterloop X Bits_x Bits_hex Start End}  % this is similar to the PowerShell strategy
        local X_now Bits_x_str Bits_hex_str
        in
            %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
            % X_now etc are cells for imperative tinkering: these initializations are very important
            X_now        = {NewCell 0}
            Bits_x_str   = {NewCell '0000000000000000'}
            Bits_hex_str = {NewCell '0000'}
            %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

            for I in Start..End do
                X_now := (A * {Array.get X (I - 1)} + C) mod M
                {Array.put X I @X_now}  % Store value of X_now at index I
                % {System.showInfo "\nX_now = "#{Value.toVirtualString @X_now 0 0}}  % for testing

                Bits_x_str := {Integer_to_bin_string @X_now}
                % {System.showInfo "Bits_x_str = "#{Value.toVirtualString @Bits_x_str 0 0}}  % for testing
                {Array.put Bits_x I @Bits_x_str}

                Bits_hex_str := {Integer_to_hex_string @X_now}
                % {System.showInfo "Bits_hex_str = "#{Value.toVirtualString @Bits_hex_str 0 0}}  % for testing
                {Array.put Bits_hex I @Bits_hex_str}
            end
        end
    end

    % own procedure needed? extra exe time?
    {Masterloop X Bits_x Bits_hex 2 End}

    {Write_to_file File_bits_x Bits_x 'bit'}
    {Write_to_file File_bits_hex Bits_hex 'byte'}

    {Application.exit 0}
end

% end of random_streams_for_perf_stats.oz
