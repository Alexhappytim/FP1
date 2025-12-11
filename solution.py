print(sum(list(map(int, str(2**1000)))))
print(max(map(lambda n:(n,(lambda f,x:f(f,x))(lambda s,x:1 if x==1 else 1+s(s,3*x+1 if x%2 else x//2),n)),range(1,10**6)),key=lambda t:t[1]))
