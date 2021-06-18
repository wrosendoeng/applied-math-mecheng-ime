%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%% TRABALHO DE MATEMATICA APLICADA A ENGENHARIA MECANICA   %%%%%%%%%
%%%%%%%%% PROGRAMA DE POS-GRADUACAO EM ENGENHARIA MECANICA - SE/4 %%%%%%%%%
%%%%%%%%% INSTITUTO MILITAR DE ENGENHARIA - IME (TURMA 2023)      %%%%%%%%%
%%%%%%%%% PROFESSOR MARCIO VIOLANTE FERREIRA                      %%%%%%%%%
%%%%%%%%% ALUNO: WALLACE RAMOS ROSENDO DA SILVA                   %%%%%%%%%
%%%%%%%%% DATA DE ENTREGA: 20/05/2021                             %%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%% ENUNCIADO: BARRA COM EXTREMIDADES MANTIDAS A T = 0      %%%%%%%%%
%%%%%%%%% ut - alpha*uxx = 0                                      %%%%%%%%%
%%%%%%%%% Condicoes Iniciais e de Contorno                        %%%%%%%%%
%%%%%%%%% u(x,0) = u0(x) = f(x) = (x, se 0 < x < L/2)             %%%%%%%%%
%%%%%%%%%                         (0, se L/2 < x < L)             %%%%%%%%%
%%%%%%%%% u(0,t) = u(L,t) = 0                                     %%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


dados = readtable('dados.xlsx');              % propriedades do material
material = input('Select a material: ','s');  % Escolha do material                              
L = double(input('Tamanho da barra 1D: '));   % Comprimento da barra 
time = double(input('Tempo de duração: '));   % Tempo máximo 
n = input('N° de divisões dos vetores: ');    % N° subdivisões nos vetores
m = input('N° de parcelas da série: ');   % N° parcelas p/ série de Fourier 
tic;                                      % Início da contagem 

switch material                  % Difusividade térmica pelo material
    case dados.material{1}       % Caso seja "Aço 1025"
        alpha = dados.alpha(1);
    case dados.material{2}       % Caso seja "Alumínio 6061"
        alpha = dados.alpha(2);
    case dados.material{3}       % Caso seja "Cobre 70-30 Níquel"
        alpha = dados.alpha(3);
    case dados.material{4}       % Caso seja "Pyrex"
        alpha = dados.alpha(4);
    case dados.material{5}       % Caso seja "Polietileno"
        alpha = dados.alpha(5);
    otherwise                    % Caso eu não selecione o material
        alpha = 1.0;
end

x = linspace(0,L,n);    % Vetor posição 
t = linspace(0,time,n); % Vetor tempo 
[X,T] = meshgrid(x,t);  % Desenvolvendo malha 2D
fft = 0;                % Ponto inicial
for k = 1 : m           % Calculando a série de Fourier 
    fft = fft + L*(2*sin(k*pi/2)-k*pi*cos(k*pi/2))*sin(k*pi*X/L)...
        .*exp(-(alpha*pi*k/L).^2.*T)/(k*pi).^2;
end

surf(X,T,fft)                                 % Superfície u(x,t)
title('HEAT PDE u_{t} - alpha*u_{xx} = 0')    % Inserindo título
xlabel('x (m)')                               % Legenda em x
ylabel('t (s)')                               % Legenda em y
zlabel('u(x,t)')                              % Legenda em z
colorbar                                      % Níveis da função

toc;                                          % Final da contagem