class RANDOM_STREAMS_FOR_PERF_STATS
-- random_streams_for_perf_stats.e for Liberty Eiffel
--
-- 2026-01-22/23
--
-- build on Ubuntu 24 LTS: $ se compile ./random_streams_for_perf_stats.e -o ./random_streams_for_perf_stats  # for development
--                         $ se compile -boost ./random_streams_for_perf_stats.e -o ./random_streams_for_perf_stats  # for production
--                           -boost: Enable all optimizations, but disable all run-time checks
--
-- run on Ubuntu 24 LTS:   $ ./random_streams_for_perf_stats
--                           with create bits_x.make_empty + bits_x.append: => real	0m0.031s
--                           with M1, K250 + create bits_x.make (M1); create bits_hex.make (K250) => real	0m0.029s
--                           same with user defined function integer_to_hex_string => real	0m0.027s
--
--                         $ sudo perf stat -r 20 ./random_streams_for_perf_stats
--                           => 0.027882 +- 0.000167 seconds time elapsed  ( +-  0.60% )
--
--
-- $ se compile --version
-- ...
-- Liberty Eiffel The GNU Eiffel Compiler, Eiffel tools and libraries
--     release 2022.dev (preparing Glenn Curtiss)
-- ...
-- $
--
-- ROPE class for efficient concatenation:
--   https://github.com/LibertyEiffel/Liberty/blob/21b081378ec12798080128e7f39878d5d2097cb7/src/lib/string/rope.e
--   => how to use this?

create {ANY}
    main

feature {} -- Initialization

    main -- Entry point
        local
            upper_limit: INTEGER
            m, a, c: INTEGER
            x: ARRAY [INTEGER]
            i, x_i: INTEGER  -- x_i: current random number x at index i: this is just avoiding x.item(index)
            M1, K250: INTEGER
            -- bits_x, bits_hex, bits_x_str, bits_hex_str, bits_hex_str2: STRING
            bits_x, bits_hex, bits_x_str, bits_hex_str: STRING
            file_bits_x, file_bits_hex: STRING
            out_file: TEXT_FILE_WRITE
            -- see from: https://github.com/LibertyEiffel/Liberty/blob/master/tutorial/io/redirection_example.e
        do
            upper_limit := 62501  -- 62501 for exactly 1M binary digits
            -- upper_limit := 50  -- for testing
            M1   := upper_limit * 16
            K250 := upper_limit * 4
            m := 65521
            a := 17364
            c := 0
            file_bits_x   := "random_bitstring.bin"
            file_bits_hex := "random_bitstring.byte"

            -- Initialize array
            create x.make (0, upper_limit)
            -- see from: https://doc.liberty-eiffel.org/api/libraries/api/liberty_core.d/storage.d/loadpath.se.d/collection.d/ARRAY.html

            -- Initialize strings
            -- create bits_x.make_empty
            create bits_x.make (M1)
            create bits_x_str.make (16)
            -- create bits_hex.make_empty
            create bits_hex.make (K250)
            create bits_hex_str.make (8)  -- outcome of .to_hexadecimal function: 33933 --> "0000848D" => should be: "848D", basically "848d"
            -- create bits_hex_str2.make (4)


            -- Initialize RNG with a random seed
            random_number_generator.next
            x_i := random_number_generator.last_integer(m - 1)
            -- last_integer(n): 1 <= Result and Result <= n
            -- see from: https://doc.liberty-eiffel.org/api/libraries/api/liberty_core.d/random.d/RANDOM_NUMBER_GENERATOR.html
            -- io.put_integer (x_i)  -- for testing

            x.put (x_i, 0)  -- start at index 0 here

            io.put_line ("%Ngenerating a random bit stream...")

            from
                i := 1
            until
                i >= upper_limit
            loop
                x_i := (a * x_i + c) \\ m  -- imperative tinkering at its finest
                x.put (x_i, i)  -- index i here
                -- io.put_string ("%N%Nx_i = ")  -- for testing
                -- io.put_integer (x_i)  -- for testing
                -- io.put_new_line  -- for testing

                -- Append 16-bit binary string
                bits_x_str := integer_to_bin_string (x_i)
                -- io.put_string (bits_x_str)  -- for testing
                bits_x.append (bits_x_str)

                -- Append 4-digit hexadecimal string
                -- bits_hex_str := x_i.to_hexadecimal
                -- https://doc.liberty-eiffel.org/api/libraries/api/liberty_core.d/numeric.d/loadpath.se.d/NATURAL_GENERAL.html#to_hexadecimal
                bits_hex_str := integer_to_hex_string (x_i)
                -- bits_hex_str2.copy_substring (bits_hex_str, 5, 8)  -- string indices start at 1!!
                -- bits_hex_str2 := bits_hex_str2.as_lower  -- have lower a...f hex digits only
                -- io.put_string ("%N" + bits_hex_str2)  -- for testing
                -- bits_hex.append (bits_hex_str2)
                -- io.put_string ("%N" + bits_hex_str)  -- for testing
                bits_hex.append (bits_hex_str)

                i := i + 1
            end


            -- io.put_string ("%N%Nbits_x = " + bits_x)  -- for testing
            -- io.put_string ("%Nbits_hex = " + bits_hex)  -- for testing


            -- Write to files
            create out_file.connect_to(file_bits_x)
            if out_file.is_connected then
                out_file.put_string(bits_x)
                out_file.disconnect
                io.put_string ("%NBit stream has been written to disk under name:  " + file_bits_x)
            else
                io.put_string ("%Ncould not write to file: " + file_bits_x)
            end

            create out_file.connect_to(file_bits_hex)
            if out_file.is_connected then
                out_file.put_string(bits_hex)
                out_file.disconnect
                io.put_string ("%NByte stream has been written to disk under name: " + file_bits_hex)
            else
                io.put_string ("%Ncould not write to file: " + file_bits_hex)
            end

            io.put_new_line
        end

------------------------------------------------------------------------------------------------------
--
-- User defined functions

feature {}

    integer_to_bin_string (n: INTEGER): STRING
    -- Return binary representation of `n` padded to 16 bits.
        local
            result_bits: STRING
            temp: INTEGER
        do
            create result_bits.make (16)
            temp := n

            from
            until
                result_bits.count = 16
            loop
                if (temp \\ 2) = 1 then
                    result_bits.prepend ("1")
                else
                    result_bits.prepend ("0")
                end

                temp := temp // 2
            end

            Result := result_bits
        end


    integer_to_hex_string (n: INTEGER): STRING
    -- Return hexadecimal representation of `n` padded to 4 digits of 0..9a..f.
        local
            hex_str: STRING
            j, k, remainder: INTEGER
        do
            create hex_str.make_filled ('0', 4)  -- Initialize with "0000" is needed here!
            j := 4
            k := n
            from
            until
                k = 0 or j = 0
            loop
                remainder := k \\ 16  -- Modulo 16
                inspect remainder
                when 0  then hex_str.put ('0', j)
                when 1  then hex_str.put ('1', j)
                when 2  then hex_str.put ('2', j)
                when 3  then hex_str.put ('3', j)
                when 4  then hex_str.put ('4', j)
                when 5  then hex_str.put ('5', j)
                when 6  then hex_str.put ('6', j)
                when 7  then hex_str.put ('7', j)
                when 8  then hex_str.put ('8', j)
                when 9  then hex_str.put ('9', j)
                when 10 then hex_str.put ('a', j)
                when 11 then hex_str.put ('b', j)
                when 12 then hex_str.put ('c', j)
                when 13 then hex_str.put ('d', j)
                when 14 then hex_str.put ('e', j)
                when 15 then hex_str.put ('f', j)
                else
                    -- Should never happen
                end
                k := k // 16  -- Integer division
                j := j - 1
            end
            Result := hex_str
        -- ensure
        --     Result.count = 4
        --     across Result as c all c.item.is_hexa_digit end
        end


    random_number_generator: RANDOM_NUMBER_GENERATOR
    -- from: https://github.com/LibertyEiffel/Liberty/blob/master/tutorial/random/example1.e
    -- Note: this is a once function in order to use always the same RANDOM_NUMBER_GENERATOR.
       once
          create {MINIMAL_RANDOM_NUMBER_GENERATOR} Result.make
       end

-- end of user defined functions
--
------------------------------------------------------------------------------------------------------

end -- class RANDOM_STREAMS_FOR_PERF_STATS
