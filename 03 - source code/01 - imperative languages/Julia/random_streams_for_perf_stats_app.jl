#=
random_streams_for_perf_stats_app.jl

2026-01-03

Run in Ubuntu 24 LTS:
$ time ./random_streams_for_perf_stats_app/build/random_streams_for_perf_stats_app/bin/random_streams_for_perf_stats_app

generating a random bit stream...
Bit stream has been written to disk under name: random_bitstring.bin
Byte stream has been written to disk under name: random_bitstring.byte

real	0m0.298s <<<<<<<<<<<<<<<<<<<<<<<<<<<<<
user	0m0.760s
sys	    0m0.061s
$


Building this project:
$ julia
julia> ]  # start package manager with this key
(@v1.12) pkg> generate random_streams_for_perf_stats_app  # only do once
(@v1.12) pkg> activate ./random_streams_for_perf_stats_app  # do every time of a code change (completely re-start Julia before!)
(@v1.12) pkg> add Random  # only do once
(@v1.12) pkg> [CTRL+C]  # leave the package manager
julia> import random_streams_for_perf_stats_app
julia> random_streams_for_perf_stats_app.julia_main()

generating a random bit stream...
Bit stream has been written to disk under name: random_bitstring.bin
Byte stream has been written to disk under name: random_bitstring.byte
0

julia>

Now, make a "standalone app":
Change into the project dir ./random_streams_for_perf_stats_app
to install the PackageCompiler:
julia> cd("./random_streams_for_perf_stats_app")
julia> using Pkg
julia> Pkg.activate(".")
  Activating project at `~/scripts/Julia/random_streams_for_perf_stats_app`

julia> Pkg.add("PackageCompiler")
...
julia>

Now compile the standalone executable:
julia> using PackageCompiler
julia> create_app(
".", # project folder
"build/random_streams_for_perf_stats_app"; # output folder
force=true
)
...
  Total library file size: 284.981 MiB
✔ [04m:25s] PackageCompiler: creating compiler sysimage (incremental=false)
✔ [02m:09s] PackageCompiler: compiling fresh sysimage (incremental=false)
Precompiling Pkg finished.
  31 dependencies successfully precompiled in 16 seconds
Precompiling packages finished.
  6 dependencies successfully precompiled in 2 seconds. 31 already precompiled.
✔ [02m:51s] PackageCompiler: compiling nonincremental system image

julia>
julia> [CTRL+Z]  # leave the Julia REPL

Run the app:
$ ./random_streams_for_perf_stats_app/build/random_streams_for_perf_stats_app/bin/random_streams_for_perf_stats_app

generating a random bit stream...
Bit stream has been written to disk under name: random_bitstring.bin
Byte stream has been written to disk under name: random_bitstring.byte
$



$ julia --version
julia version 1.12.3
$

=#


module random_streams_for_perf_stats_app

    import Random


    # Google AI
    # Your julia_main must accept 0 or 1 argument (String vector)
    function julia_main()::Cint
        try
            # Your application logic here
            real_main()
        catch
            Base.invokelatest(Base.display_error, Base.catch_stack())
            return 1 # Return non-zero on error
        end
        return 0 # Return 0 for success
    end


    function real_main()
        END = 62501  # 62501 for exactly 1M binary digits; const END has no speed effect here
        # END = 11  # for testing

        m = 65521  # = 2^16 - 15
        a = 17364
        c = 0

        file_bits_x   = "random_bitstring.bin"
        file_bits_hex = "random_bitstring.byte"


        x = Array{UInt32}(undef, END)
        x[1] = rand(1:m-1)  # indexing in Julia is 1-based; m is inclusive
        # 2026-01-03: it's very important to have also above definitions inside real_main()
        # to always have a new random seed with each program run!
        # println("x[1] = ", x[1])  # for testing

        # create large arrays of strings:
        bits_x   = fill("000000000000", END-1)
        bits_hex = fill("0000", END-1)


        println("\ngenerating a random bit stream...")
        for i in 2:END
            h = i - 1
            # println("h = $h")  # for testing
            x[i] = (a*x[h] + c) % m
            # println("x[i] = ", x[i])  # for testing

            bits_x_str0 = string(x[i], base=2)
            bits_x_str  = lpad(bits_x_str0, 16, "0")  # padding: https://www.jlhub.com/julia/manual/en/function/lpad
            # println("  bits_x_str =  ", bits_x_str)  # for testing
            bits_x[h]   = bits_x_str

            bits_hex_str0 = string(x[i], base=16)
            bits_hex_str  = lpad(bits_hex_str0, 4, "0")
            # println("  bits_hex_str =  ", bits_hex_str)  # for testing
            bits_hex[h]   = bits_hex_str
        end


        bits_x_str_total   = join(bits_x)
        # println("\nbits_x_str_total = ", bits_x_str_total)  # for testing
        bits_hex_str_total = join(bits_hex)
        # println("\nbits_hex_str_total = ", bits_hex_str_total)  # for testing


        # try to write the string to the file with error handling: Duck.ai
        try
            # Open the file for writing
            open(file_bits_x, "w") do file
                write(file, bits_x_str_total)
            end
            println("Bit stream has been written to disk under name: ", file_bits_x)
        catch e
            println("could not write to file: ", file_bits_x, " -- ", e)
        end

        try
            # Open the file for writing
            open(file_bits_hex, "w") do file
                write(file, bits_hex_str_total)
            end
            println("Byte stream has been written to disk under name: ", file_bits_hex)
        catch e
            println("could not write to file: ", file_bits_hex, " -- ", e)
        end

    end  # function main_()

end  # module


# end of random_streams_for_perf_stats_app.jl
