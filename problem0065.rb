class Fraction
    attr_accessor :numerator, :denominator
    def initialize numerator, denominator
        @numerator = numerator
        @denominator = denominator
    end
    def value
        1.0 * @numerator / @denominator
    end
end

def sum_of_digits number
    return 0 if number == 0
    return (number % 10) + sum_of_digits(number.div(10))
end

xs = [2, (1..33).map{|x| [1,2*x,1]}].flatten.reverse
result = xs.inject(Fraction.new(1, xs.shift)){|prev, x| 
    Fraction.new x * prev.numerator + prev.denominator, prev.numerator}

puts "#{result.inspect} = #{result.value}"
puts "sum of numerator's digits: #{sum_of_digits(result.numerator)}"

