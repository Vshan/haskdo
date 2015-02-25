read_file = File.open("arg.txt")
file_to_array = read_file.to_a
File.open('temp.rb', 'w') do |f|
  file_to_array.each_with_index do |l, i|
    cur_line = l
    next_line = file_to_array[i+1]
    start_time = cur_line.partition(" ").first
    fin_time = next_line.partition(" ").first
    desc = cur_line.partition(" ").last
    new_line = start_time + " " + fin_time + " " + desc
    f.puts new_line
  end
end
