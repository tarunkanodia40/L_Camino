# def is_variable

def parse(v)
    n = v.length
    if n<=0
        return 0 
    end
    # rule 1
    if n == 1 
        if v[0]>='a' and v[0]<='z'
            return 1
        else 
            return 0
        end 
    end 
    if v[0] == '('
        printf "2=>  %s\n",v
        if n<5 or v[n-1] != ')' or v[1]!="\\" or not(v[2]>='a' and v[2]<='z') or v[3]!='.'
            return 0
        end 
        return parse(v[4..(n-2)])
    end
    if v[0] == '['
        if v[n-1]!=']'
            return 0
        end
        printf "1=>  %s\n",v
        (1..n-2).each do |i|
            if v[i]==']' and v[i+1]=='['
                if parse(v[1..(i-1)])==1 and parse(v[(i+2)..(n-2)])==1
                    printf "Ans<=> %d\n",i
                    return 1
                end
            end
        end 
        return 0
    end
    return 0
end

def remove_from_list(v,e)
    v2 = Array.new
    n = v.length
    (0..(n-1)).each do |i|
        if v[i]!=e 
            v2.append(v[i])
        end 
    end
    return v2 
end    

def merge_lists(v1,v2)
    ans = Array.new
    printf "\n---v1--\n"
    puts v1 
    printf "\n---v2--\n"
    puts v2 
    printf "\n---v3--\n"
    n = v1.length
    (0..(n-1)).each do |i| 
        ans.append(v1[i])
    end
    m = v2.length
    (0..(m-1)).each do |i| 
        already_present = 0
        (0..(n-1)).each do |j|
            if v2[i]==v1[j]
                already_present = 1
            end 
        end 
        if already_present==0
            ans.append(v2[i])
        end
    end 
    return ans
end

def free_vars(v)
    n = v.length
    ans = Array.new
    if n<=0
        return 0 ,ans
    end
    # rule 1
    if n == 1 
        if v[0]>='a' and v[0]<='z'
            ans.append(v[0])
            puts ans
            return 1,ans 
        else 
            return 0,ans
        end 
    end 
    if v[0] == '('
        printf "2=>  %s\n",v
        if n<5 or v[n-1] != ')' or v[1]!="\\" or not(v[2]>='a' and v[2]<='z') or v[3]!='.'
            return 0,ans
        end 
        a,b = free_vars(v[4..(n-2)])
        if a == 1
            return a,remove_from_list(b,v[2])
        else 
            return a,ans
        end 
    end
    if v[0] == '['
        if v[n-1]!=']'
            return 0,ans
        end
        printf "1=>  %s\n",v
        (1..n-2).each do |i|
            if v[i]==']' and v[i+1]=='['
                a1, b1 = free_vars(v[1..(i-1)])
                a2, b2 = free_vars(v[(i+2)..(n-2)])
                puts(b2)
                if a1==1 and a2==1
                    printf "Ans<=> %d\n",i
                    return 1,merge_lists(b1,b2)
                end
            end
        end 
        return 0,ans
    end
    return 0,ans
end

def substitution(lambda_term,x,replace_term)
    n = lambda_term.length
    ans = String.new
    if n<=0
        return 0 ,ans
    end
    # rule 1
    if n == 1 
        if lambda_term[0]>='a' and lambda_term[0]<='z'
            if x==lambda_term[0]
                ans=replace_term # x[x:= N] = N
            else 
                ans = lambda_term # y[x:= N] = y
            end
            return 1,ans 
        else 
            return 0,ans
        end 
    end 
    if lambda_term[0] == '('
        printf "2=>  %s\n",lambda_term
        if n<5 or lambda_term[n-1] != ')' or lambda_term[1]!="\\" or not(lambda_term[2]>='a' and lambda_term[2]<='z') or lambda_term[3]!='.' or parse(lambda_term[4..(n-2)])==0
            return 0,ans
        end 
        if x==lambda_term[2]
            return 1, lambda_term # (\x.M)[x:= N] = (\x.M) as x is not free in the lambda term
        end
        a1,b1 = free_vars(replace_term)
        is_in_free_var = false 
        b1_size = b1.length
        (0..(b1_size-1)).each do |i|
            if lambda_term[2]==b1[i]
                is_in_free_var = true
            end
        end 
        if is_in_free_var == false 
            # (\y.M)[x:= N] = (\y.M[x:=N]) in case if y is not free in the lambda term N
            a2 , b2 = substitution(lambda_term[4..(n-2)],x,replace_term)
            if a2 == 1
                return a2,lambda_term[0..3] + b2 + lambda_term[-1..-1]
            else 
                return a2,b2
            end 
        end
        variables_used = [x]
        (0..(n-1)).each do |i| 
            if lambda_term[i]>='a' and lambda_term[i]<='z'
                already_present = false
                (0..(variables_used.length-1)).each do |j|
                    if variables_used[j] == lambda_term[i]
                        already_present=true 
                    end 
                end 
                if already_present==true 
                    variables_used.append(lambda_term[i])
                end 
            end 
        end 
        (0..(replace_term.length-1)).each do |i| 
            if replace_term[i]>='a' and replace_term[i]<='z'
                already_present = false
                (0..(variables_used.length-1)).each do |j|
                    if variables_used[j] == replace_term[i]
                        already_present=true 
                    end 
                end 
                if already_present==true 
                    variables_used.append(replace_term[i])
                end 
            end 
        end 
        usefull_variable = lambda_term[2]
        ("a".."z").each do |i|
            already_present = false
            (0..(variables_used.length-1)).each do |j|
                if variables_used[j] == i 
                    already_present = true 
                end 
            end
            if already_present==false 
                usefull_variable = i 
            end 
        end 
        updated_string = lambda_term
        replace_char = lambda_term[2]
        (0..(n-1)).each do |i|
            if updated_string[i] == replace_char
                updated_string[i] = usefull_variable 
            end 
        end 
        a2 , b2 = substitution(updated_string[4..(n-2)],x,replace_term)
        if a2 == 1
            return a2,updated_string[0..3] + b2 + updated_string[-1..-1]
        else 
            return a2,b2
        end 
        
    end
    if lambda_term[0] == '['
        if lambda_term[n-1]!=']'
            return 0,ans
        end
        printf "1=>  %s\n",lambda_term
        (1..n-2).each do |i|
            if lambda_term[i]==']' and lambda_term[i+1]=='['
                a1, b1 = substitution(lambda_term[1..(i-1)],x,replace_term)
                a2, b2 = substitution(lambda_term[(i+2)..(n-2)],x,replace_term)
                puts(b2)
                if a1==1 and a2==1
                    printf "Ans<=> %d\n",i
                    ans = "[" + b1 + "][" + b2 + "]" # MP[x:=N] = M[x:=N] P[x:=N]
                    return 1,ans
                end
            end
        end 
        return 0,ans
    end
    return 0,ans
end

def do_beta_reduction(v)
    n = v.length
    ans = String.new
    if n<=0
        return 0 ,ans
    end
    if n == 1 
        if v[0]>='a' and v[0]<='z'
            ans = v
            return 1,ans 
        else 
            return 0,ans
        end 
    end 
    if v[0] == '('
        # printf "2=>  %s\n",v
        if n<5 or v[n-1] != ')' or v[1]!="\\" or not(v[2]>='a' and v[2]<='z') or v[3]!='.' 
            return 0,ans
        end
        a1,b1 = do_beta_reduction(v[4..(n-2)])
        if a1 == 1
            printf "Type 2 => %s,%s", v ,v[0..3] + b1 + + v[-1..-1]
            return a1, v[0..3] + b1 + + v[-1..-1]
        else 
            return a1,b1 
        end 
    end
    if v[0] == '['
        if v[n-1]!=']'
            return 0, ans
        end
        # printf "1=>  %s\n",v
        (1..n-2).each do |i|
            if v[i]==']' and v[i+1]=='['
                a1, b1 = do_beta_reduction(v[1..(i-1)])
                a2 = parse( v[(i+2)..(n-2)])
                # printf "%s => %s \n",v[1..(i-1)],b1
                b2 = v[(i+2)..(n-2)]
                # puts(b2)
                if a1==1 and a2==1
                    if b1[0]=="("
                        ## we have the first term as Abs, doing substitution 
                        a12, b12 = substitution(b1[4..(b1.length-2)],b1[2],b2)
                        printf "%s <<<<%s>>> %s \n",v[1..(i-1)],b2,b12
                        if a12 == 1
                            printf "%s => %s \n",v[1..(i-1)],b1
                            return do_beta_reduction(b12)
                        else
                            return a12,ans
                        end
                    else 
                        a122 , b122 = do_beta_reduction(b2)
                        return 1, "["+b1+"]["+b122+"]"
                    end
                end
            end
        end 
        return 0
    end    
end

input = gets.chomp
a,b= do_beta_reduction(input)
# output = parse(input)
# printf "%d\n",output
# a,b = substitution(input,"x","(\\a.y)")
printf "\n---Free Vars---\n\n"
puts b
