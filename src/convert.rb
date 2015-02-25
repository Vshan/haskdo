# This ruby script converts the user's log file to another
# log file which includes the time spent on a certain task.
# While the original file only included the end point,
# this converted log file contains the start point as well.

# This script needs to be run with an argument which should
# be the file name of the user's original log file.

read_file = File.open("#{ARGV[0]}")
file_to_array = read_file.to_a
File.open("temp.txt", "w") do |f|
  file_to_array.each_with_index do |l, i|
    # Since we don't want the last sleep part
    unless (i == file_to_array.size - 1)
      cur_line = l
      next_line = file_to_array[i+1]
      start_time = cur_line.partition(" ").first
      fin_time = next_line.partition(" ").first
      desc = cur_line.partition(" ").last
      new_line = start_time + " " + fin_time + " " + desc
      f.puts new_line
    end
  end
end
read_file.close
