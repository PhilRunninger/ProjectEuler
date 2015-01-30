require 'benchmark'

puts Benchmark.measure { puts (('0'..'9').to_a << '').map{|middle| (('1'..'999').to_a << '').map{|half| "#{half}#{middle}#{half.reverse}".to_i}}.flatten.delete_if{|x| x>999999 || x.to_s(2)!=x.to_s(2).reverse}.inject(:+) }

puts Benchmark.measure { puts (('0'..'9').to_a << '').map{|middle| (('1'..'999').to_a << '').map{|half| "#{half}#{middle}#{half.reverse}".to_i}}.flatten.keep_if{|x| x<999999 && x.to_s(2)==x.to_s(2).reverse}.inject(:+) }
