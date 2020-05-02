# Ruby: Testing the behavior of various control flow constructs inside blocks

def iter
  puts "start iter"
  ret = (1..5).each do |x|
    yield x
  end
  puts "end iter, each returned #{ret}"
  "retval from iter"
end

def get_block(&b) b end

def call_block(&b)
  puts "start call_block"
  ret = b.call
  puts "end call_block, block returned #{ret}"
  "retval from call_block"
end

# `break` ends execution of the function called with the current block,
# and sets the return value of the function (like `return` executed in the
# context of function called with block)
puts "\nbreak"
puts (iter do |i|
  break "end" if i == 3
  puts i
end)

# Works the same when reified as a proc
puts "\nbreak with call_block"
puts call_block{break "foo"}

# `break` is invalid after the function is finished
# get_block{break}.call
#  -> break from proc-closure (LocalJumpError)

# `next` 
puts "\nnext"
puts (iter do |i|
  next if i == 3
  puts i
end)

puts "\nredo"
redone = false
puts (iter do |i|
  puts i
  if i == 3 && !redone
    redone = true
    redo
  end
end)

puts "\nreturn"
puts (iter do |i|
  return if i == 3
  puts i
end)
