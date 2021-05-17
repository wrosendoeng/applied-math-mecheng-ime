dados = readtable('dados.xlsx');              % Lendo dados de propriedades do material
material = input('Select a material: ','s');  % Difusividade térmica do material
m = input('Número de parcelas da série: ');   % Quantas parcelas a serem somadas na série de Fourier                               
L = double(input('Tamanho da barra 1D: '));   % Comprimento da barra em metros
time = double(input('Tempo de duração: '));   % Tempo máximo de distribuição em segundos
tic;                                          % Início da contagem do processamento do código

switch material                               % Selecionar a difusividade térmica de acordo com o material
    case string(table2cell(dados(1,1)))       % Caso seja "Aço 1025"
        alpha = table2array(dados(1,5))/1e3;
    case string(table2cell(dados(2,1)))       % Caso seja "Alumínio 6061"
        alpha = table2array(dados(2,5))/1e3;
    case string(table2cell(dados(3,1)))       % Caso seja "Cobre 70-30 Níquel"
        alpha = table2array(dados(3,5))/1e3;
    otherwise                                 % Caso eu não selecione o material, usando um valor genérico
        alpha = 1.0;
end

x = linspace(0,L,100);                        % Vetor posição com subdivisões linearmente espaçadas
t = linspace(0,time,100);                     % Vetor tempo com subdivisões linearmente espaçadas

[X,T] = meshgrid(x,t);                        % Desenvolvendo malha 2D
for k = 1 : m                                 % Calculando a série de Fourier de acordo com o número de parcelas
    fft = fft + 2*L*(sin(k*pi/2)-k*pi*cos(k*pi/2))*sin(k*pi*X/L).*exp(-T*(alpha*pi*k/L)^2)./(k*pi).^2;
end

surf(X,T,fft)                                 % Desenvolvendo o gráfico com a superfície u(x,t)
title('HEAT PDE u_{t} - alpha*u_{xx} = 0')    % Inserindo título
xlabel('x (m)')                               % Legenda em x
ylabel('t (s)')                               % Legenda em y
zlabel('u(x,t)')                              % Legenda em z
colorbar                                      % Barra colorida para identificar os níveis da função

toc;                                          % Final da contagem do processamento do código