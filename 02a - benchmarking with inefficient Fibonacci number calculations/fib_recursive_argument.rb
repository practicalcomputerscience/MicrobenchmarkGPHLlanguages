=begin
fib_recursive_argument.rb

2026-02-10

test on Ubuntu 24 LTS with Ruby 3.2.10, installed with rvm (Ruby Version Manager): OK


output:
  1/ interpreting YARV VM bytecode:
     $ ruby ./fib_recursive_argument.rb <n>

     n = 44 => fib = 701408733           => Time: 35.673556658s
     ...
     n = 47 => fib = 2971215073          => Time: 150.435094667s


  2/ MJIT compiled YARV VM bytecode:
     $ ruby --mjit ./fib_recursive_argument.rb <n>

     n = 44 => fib = 701408733           => Time: 11.684656705s
     ...
     n = 47 => fib = 2971215073          => Time: 50.134606918s

  2/ YJIT compiled YARV VM bytecode:
     $ ruby --yjit ./fib_recursive_argument.rb <n>

     n = 44 => fib = 701408733           => Time: 9.3817913s
     ...
     n = 47 => fib = 2971215073          => Time: 39.757120718s


$ ruby --version
ruby 3.2.10 (2026-01-14 revision a3a6d25788) [x86_64-linux]
$

=end


def fib(n)
  return n if n <= 1
  fib(n - 1) + fib(n - 2)
end


first_arg, *the_rest = ARGV
n = Integer(first_arg)
if n < 2
  return
end

puts "argument n = #{n}"

start_time = Time.now
puts fib(n)
end_time = Time.now

elapsed_time = end_time - start_time
puts "Time: #{elapsed_time}s"

# end on fib_recursive_argument.rb

