for i = 1 : length(x)
    if x(i) < 0.5*L
        y(i) = x(i);
    else
        y(i) = 0.0;
    end
end

plot(x,y,'b--o')
title('y = x, se 0<x<L/2, senão y = 0')
xlabel('x')
ylabel('y')
legend('f(x)')