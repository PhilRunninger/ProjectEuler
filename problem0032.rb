# 1*2345=6789
# 12*345=6789
# 1*2=3456789 -> 1234567*8=9

x = []
(1..9).to_a.permutation.to_a.each do |perm|
    (0..6).each do |ndigits|
        (ndigits+1..7).each do |mdigits|
            n  = (0..ndigits).inject(0) {|sum,i| sum+perm[i]*10**(ndigits-i)}
            m  = (ndigits+1..mdigits).inject(0) {|sum,i| sum+perm[i]*10**(mdigits-i)}
            nm = (mdigits+1..8).inject(0) {|sum,i| sum+perm[i]*10**(8-i)}
            puts "#{n} * #{m} = #{nm}" if n * m == nm
            x << nm if n * m == nm
        end
    end
end
puts x.uniq.reduce(:+)
