# This script converts the HTML file to something more readable

counter = false
read_file = File.open("#{ARGV[0]}")
file_to_array = read_file.to_a
File.open("tolog.txt", "w") do |f|
  file_to_array.each_with_index do |l, i|
    l.split("\"").each do |dl|
     if (dl =~ /A Daily Log of Time/)
       if (counter)
         dl[21..-1].split("\\n").each do |ll|
           f.puts ll
         end
       end
       counter = true
     end
    end
  end
end
read_file.close
