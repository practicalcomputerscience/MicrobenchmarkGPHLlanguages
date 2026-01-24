class RANDOM_BITSTRING_AND_FLEXIBLE_PASSWORD_GENERATOR
-- random_bitstring_and_flexible_password_generator.e for Liberty Eiffel
--
-- 2026-01-23/24
--
-- build on Ubuntu 24 LTS: $ se compile ./random_bitstring_and_flexible_password_generator.e -o ./random_bitstring_and_flexible_password_generator  # for development
--                         $ se compile -boost ./random_bitstring_and_flexible_password_generator.e -o ./random_bitstring_and_flexible_password_generator  # for production
--                           -boost: Enable all optimizations, but disable all run-time checks
--
-- run on Ubuntu 24 LTS:   $ ./random_bitstring_and_flexible_password_generator
--
--
-- $ se compile --version
-- ...
-- Liberty Eiffel The GNU Eiffel Compiler, Eiffel tools and libraries
--     release 2022.dev (preparing Glenn Curtiss)
-- ...
-- $

create {ANY}
    main

feature {} -- Initialization

    main -- Entry point
        local
            upper_limit: INTEGER
            m, a, c: INTEGER
            x: ARRAY [INTEGER]
            i, x_i, n_char, j: INTEGER  -- x_i: current random number x at index i: this is just avoiding x.item(index)
            M1, K250: INTEGER
            bits_x, bits_hex, bits_x_str, bits_hex_str: STRING
            file_bits_x, file_bits_hex: STRING

            out_file: TEXT_FILE_WRITE
            -- see from: https://github.com/LibertyEiffel/Liberty/blob/master/tutorial/io/redirection_example.e

            answer, with_special_chars: BOOLEAN
            reply, bin0, bin0_0, bin0_1, pw_chars: STRING
            char0, char1: INTEGER
            char0a, char1a: CHARACTER

            reg_expr_builder: REGULAR_EXPRESSION_BUILDER
            pattern: REGULAR_EXPRESSION
            -- see from: https://github.com/LibertyEiffel/Liberty/blob/21b081378ec12798080128e7f39878d5d2097cb7/src/lib/regular_expression/regular_expression_builder.e#L7
            --   posix_builder, perl5_builder, python_builder
            -- https://github.com/LibertyEiffel/Liberty/blob/21b081378ec12798080128e7f39878d5d2097cb7/tutorial/regular_expression/example2.e#L18

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
            create bits_x.make (M1)
            create bits_x_str.make (16)
            create bits_hex.make (K250)
            create bits_hex_str.make (8)

            -- Initialize RNG with a random seed
            random_number_generator.next
            x_i := random_number_generator.last_integer(m - 1)
            -- last_integer(n): 1 <= Result and Result <= n
            -- see from: https://doc.liberty-eiffel.org/api/libraries/api/liberty_core.d/random.d/RANDOM_NUMBER_GENERATOR.html
            -- io.put_integer (x_i)  -- for testing

            x.put (x_i, 0)  -- start at index 0 here

            io.put_string ("%Ngenerating a random bit stream...")

            from
                i := 1
            until
                i >= upper_limit
            loop
                x_i := (a * x_i + c) \\ m  -- imperative tinkering at its finest
                x.put (x_i, i)  -- index i here
                -- io.put_string ("%N%Nx_i = " + x_i.out)  -- for testing

                -- Append 16-bit binary string
                bits_x_str := integer_to_bin_string (x_i)
                -- io.put_string ("%N" + bits_x_str)  -- for testing
                bits_x.append (bits_x_str)

                -- Append 4-digit hexadecimal string
                bits_hex_str := integer_to_hex_string (x_i)
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


            -- make a password of N_CHAR printable chars:
            n_char := 12
            answer := False
            from
            until
                answer
            loop
                io.put_string("%NPassword of " + n_char.out + " printable chars OK? 'y' or another integer >= 8: ")
                io.read_line
                reply := io.last_string

                if reply.is_equal("y") then
                    answer := True
                else
                    if reply.is_integer and then reply.to_integer >= 8 then
                        n_char := reply.to_integer
                        answer := True
                    else
                        io.put_string("enter an integer number >= 8 or 'y'%N")
                    end
                end
            end
            -- io.put_string ("%Nn_char = " + n_char.out)  -- for testing


            with_special_chars := True
            answer := False
            from
            until
                answer
            loop
                io.put_string("%NDo you want me to use special characters like .;,+*... ? 'y' or 'n': ")
                io.read_line
                reply := io.last_string

                if reply.is_equal("y") then
                    answer := True
                else
                    with_special_chars := False
                    answer := True
                end
            end
            -- io.put_string ("%Nwith_special_chars = " + with_special_chars.out)  -- for testing


            if with_special_chars then
                pattern := reg_expr_builder.convert_perl_pattern("[!-~]")
            else
                pattern := reg_expr_builder.convert_perl_pattern("[A-Za-z0-9]")
            end
            -- io.put_string ("%Npattern = " + pattern.out)  -- for testing


            j := 0  -- counter in x[j]
            create pw_chars.make (n_char)
            create bin0_0.make (8)
            create bin0_1.make (8)

            from
                i := 0  -- char counter in password
            invariant
                i <= n_char
            until
                i >= n_char
            loop
                -- io.put_string ("%N%Nx(j) = " + x.item(j).out)  -- for testing
                bin0 := integer_to_bin_string (x.item(j))
                -- io.put_string ("%Nbin0 = " + bin0)  -- for testing

                bin0_0.copy_substring (bin0, 1, 8)
                bin0_1.copy_substring (bin0, 9, 16)
                -- io.put_string ("%Nbin0_0 = " + bin0_0)  -- for testing
                -- io.put_string ("%Nbin0_1 = " + bin0_1)  -- for testing

                char0 := bin_string_to_integer(bin0_0)
                char1 := bin_string_to_integer(bin0_1)
                -- io.put_string ("%Nchar0 = " + char0.out)  -- for testing
                -- io.put_string ("%Nchar1 = " + char1.out)  -- for testing

                char0a := char0.to_character
                char1a := char1.to_character
                -- io.put_string ("%Nchar0a = " + char0a.out)  -- for testing
                -- io.put_string ("%Nchar1a = " + char1a.out)  -- for testing

                if pattern.match(char0a.out) then
                    pw_chars.append(char0a.out)
                    i := i + 1
                end

                if pattern.match(char1a.out) and i < n_char then
                    pw_chars.append(char1a.out)
                    i := i + 1
                end

                j := j + 1
            end

            io.put_string ("%NYour password of " + n_char.out + " characters is: " + pw_chars)
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
            create hex_str.make_filled ('0', 4)  -- Initialization with "0000" is needed here!
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


    bin_string_to_integer (binary_string: STRING): INTEGER
    -- a Duck.ai solution on prompt:
    -- "Parse a string with the binary representation of an integer number and convert it into an integer in Liberty Eiffel language."
        local
            binary_as_integer: INTEGER
            base: INTEGER
        do
            from
                binary_as_integer := 0
                base := 1  -- This represents 2^0
            until
                binary_string.is_empty
            loop
                if binary_string.last = '1' then
                    binary_as_integer := binary_as_integer + base
                end
                binary_string.remove_last  -- Remove the last character for the next iteration
                base := base * 2  -- Move to the next position (2^n)
            end
            Result := binary_as_integer
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

end -- class RANDOM_BITSTRING_AND_FLEXIBLE_PASSWORD_GENERATOR
