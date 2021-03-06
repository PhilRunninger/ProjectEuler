# You are given the following information, but you may prefer to do some research for yourself.
# * 1 Jan 1900 was a Monday.
#    Thirty days has September,
#    April, June and November.
#    All the rest have thirty-one,
#    Saving February alone,
#    Which has twenty-eight, rain or shine.
#    And on leap years, twenty-nine.
# * A leap year occurs on any year evenly divisible by 4, but not on a century unless it is divisible by 400.
# How many Sundays fell on the first of the month during the twentieth century (1 Jan 1901 to 31 Dec 2000)?

# Here it is without Date class objects.
count = 0
day = 1
(1900..2000).each do |year|
  (0..11).each do |month|
    day = day % 7
    count += 1 if day == 0 and year >= 1901
    day += [31,28,31,30,31,30,31,31,30,31,30,31][month]
    day += 1 if month==1 && year % 4 == 0 && (year % 400 == 0 || year % 100 > 0)
  end
end
puts "There are #{count} Sundays on the first of the month."
