count = 0
day = 1
(1900..2000).each do |year|
  (0..11).each do |month|
    day = day % 7
    count += 1 if day == 0 and year >= 1901
    day += [31,28,31,30,31,30,31,31,30,31,30,31][month]
    day += 1 if month==1 and year % 4 == 0 && (year % 400 == 0 || year % 100 > 0)
  end
end
puts "There are #{count} Sundays on the first of the month."
