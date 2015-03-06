def continued_fraction(x)
    out = []
    100.times { |i|
        n = x.floor
        puts "#{i}:  #{n}"
        out << n
        x = x - n
        x = 1 / x
    }
end

continued_fraction(Math.sqrt(23))
