+++
title = 'ODE-by-Matlab-01-人口增长模型'
date = 2025-07-07T09:16:05+08:00
draft = false
mathkatex = true
categories = ['matlab', 'ode']
tags = ['matlab', 'ode', 'malthus', 'logistic', 'infection', 'population']
toc = true
tocBorder = true
+++


## 马尔萨斯模型

马尔萨斯模型是人口增长模型中最简单的模型，它由英国牧师家马尔萨斯在1798年提出。
他利用在教堂工作的机会，收集英国100多年的人口数据，发现人口的**相对增长率**是常数。
在这个基础上，建立了一个描述人口增长的模型，也就是著名的“马尔萨斯人口模型”。

在这个模型中，最重要的额概念是**相对增长率**。

$$
\frac{\dot{u}}{u} = r
$$

这里就涉及到微分的概念，变量$u$对时间$t$的导数，也就是变化率(这里可以称之为增长率)，记作$\dot{u}$。

$$
\dot{u} = \frac{du}{dt}
$$

那么“马尔萨斯人口模型”表达为微分方程是这样的形式。

$$
\frac{\frac{du}{dt}}{u} = \alpha  \rightarrow \frac{du}{dt} = \alpha u
$$

其中，时间为$t$，人口$u$为依赖于时间的函数，相对增长率是$\alpha$（$\alpha >0$）。

这个方程的解很容易通过不定积分求解。

$$
\int \frac{du}{u} = \int \alpha dt \rightarrow \ln u = \alpha t + C \rightarrow u(t) = e^{\alpha t + C} = n e^{\alpha t}
$$

这个解是一个指数函数！众所周知，指数函数的增长是非常快的。这在一定的程度上导致了社会主义国家考虑对人口增长进行控制。

```matlab
n = 81;
t = linspace(0, 100, 100);
alpha = 0.02; % 人口增长率
% 马尔萨斯人口模型
u = n * exp(alpha * t);
figure;
plot(t, u , 'b-', 'LineWidth', 2);
xlabel('时间(年)');
ylabel('人口(亿)');
title('马尔萨斯人口模型')
grid on;
% 保存图像  
exportgraphics(gcf, 'chp01/malthus_population_model.png', 'Resolution', 300);
```

![png](/matlab/ode/from-malthus-to-logistic_files/from-malthus-to-logistic_1_0.png)

## UN预测数据

我们可以从[UN官网](<https://population.un.org/wpp/downloads?folder=Standard%20Projections&group=Most%20used>)下载人口预测数据。
数据的格式是csv，第二表格包含了世界各国从2025年到2100年的人口预测数据。

我们通过`websave`命令下载数据， 并用`readtable`命令读取数据。注意实际的数据位置、文件名可能会发生改变，需要根据实际情况修改。

```matlab
UN_Projection_File = 'https://population.un.org/wpp/assets/Excel%20Files/1_Indicator%20(Standard)/EXCEL_FILES/1_General/WPP2024_GEN_F01_DEMOGRAPHIC_INDICATORS_COMPACT.xlsx';
% 下载UN人口预测数据
if ~exist('chp01/WPP2024_GEN_F01_DEMOGRAPHIC_INDICATORS_COMPACT.xlsx', 'file')
    websave('chp01/WPP2024_GEN_F01_DEMOGRAPHIC_INDICATORS_COMPACT.xlsx', UN_Projection_File);
end

% 读取UN人口预测数据, sheet 'Medium variant', range 'L18:94': total population, K18:94: Years
if ~exist('chp01/WPP2024_GEN_F01_DEMOGRAPHIC_INDICATORS_COMPACT.xlsx', 'file')
    error('UN人口预测数据文件未找到，请检查下载路径。');
end
data = readtable('chp01/WPP2024_GEN_F01_DEMOGRAPHIC_INDICATORS_COMPACT.xlsx', ...
    'Sheet', 'Medium variant', 'Range', 'K18:L94');
```

然后我们来看看，马尔萨斯模型数据与UN预测数据的差异。

```matlab
% 提取年份和人口数据
years = data{:, 1};
population = data{:, 2} * 1e3 * 1e-8; % 单位为千人-->亿

% 绘制UN人口预测数据
figure; 
plot(years, population, 'k-', 'LineWidth', 2);
xlabel('年份');
ylabel('人口(亿)');
title('联合国人口预测');
grid on;
legends = {'UN Population Projection'};

hold on;
% 绘制拟合的线性模型

alphas = linspace(0.001, 0.01, 5); % 人口增长率范围

for idx = 1:length(alphas)
    alphai = alphas(idx);
    % 使用马尔萨斯模型拟合UN人口预测数据
    iyears = years - years(1); % 将年份归一化
    u = population(1) * exp(alphai * iyears); % 初始人口为第一个年份的人口    
    % 绘制拟合曲线
    plot(years, u, 'DisplayName', ['alpha = ', num2str(alphai)], 'LineWidth', 1.0);
    legends{end + 1} = ['alpha = ', num2str(alphai)]; %#ok<*SAGROW>
end

legend(legends, 'Location', 'best');

% 保存图像
exportgraphics(gcf, 'chp01/un_population_projection.png', 'Resolution', 300);
```

![png](/matlab/ode/from-malthus-to-logistic_files/from-malthus-to-logistic_5_0.png)

增长率从0.001到0.01的马尔萨斯模型预测数据跟UN的预测有很大的区别，从图上看趋势都是不正确的。
联合国的预测中，人口会达到一个饱和值（峰值），并在其后缓慢下降。

本质上来讲，马尔萨斯模型仅仅考虑人口相对增长率的线性特征，没有考虑非线性的饱和特征。也就是，在人口较少时，人口的增长所受的限制很少，能够出现指数增长；而当人口达到一定的数量是，生存环境、生态资源、社会因素等都会对人口增长产生限制，导致人口增长率逐渐减小，最终趋近于0。

这就提示我们需要考虑一个非线性的模型来描述人口增长。

## Logistic模型

Logistic模型为什么叫做Logistic模型呢？因为它的解是一个Logistic函数。什么叫Logistic函数呢？它的形式是这样的：

$$
f(x) = \frac{L}{1 + e^{-k(x - x_0)}}
$$

这里$L$是函数的最大值，$k$是增长率，$x_0$是函数的中点。

```matlab
figure;
% Plot Logistic function
t = linspace(0, 100, 100);
L = 10; % 最大人口
K = 0.1; % 资源限制

plot(t, L ./ (1 + exp(-K * (t - 50))), 'r-', 'LineWidth', 2);
xlabel('时间(年)');
ylabel('人口(亿)');
title('Logistic Population Model');
grid on;

% 保存图像
exportgraphics(gcf, 'chp01/logistic_population_model.png', 'Resolution', 300);
```

![png](/matlab/ode/from-malthus-to-logistic_files/from-malthus-to-logistic_7_0.png)

至于这个函数为什么叫做Logistic函数呢？

因为法国数学家 Pierre François Verhulst就是这么命名的，他在1845年的[论文（居然还有电子版可以看……）](<http://resolver.sub.uni-goettingen.de/purl?PPN129323640_0018>)提出了这个函数来描述人口增长。这个论文现在还可以看到电子版，是法语的。在这论文电子版的地54也，作者比较了他的模型和马尔萨斯模型，并在图上给出了Logarithmique vs. Logistique的曲线命名。

<p align="center">
    <img src="/matlab/ode/from-malthus-to-logistic_files/logistic.png" alt="Logistic函数" width="400px" />
</p>

Verhulst在论文里面写到：Nous donnerons le nom de logistique à la courbe (voyez la figure) caractérisée par l'équation précédente. 就是什么我们给前述方程给出的曲线（如图）命名为Logistic曲线之类……

说到底就是他给这个函数起了个名字……

我看了好多地方，都没有找到为什么叫logistique……

反正这个跟后勤（logistics）没有关系。国外网友猜测这个跟logistique的词源有关，logistique是法语，源自拉丁语logisticus，意思是“计算的”，而这个函数的计算……我编不出来了……

### Verhulst的原始推导

现在我们来看看Verhulst是如何推导出Logistic模型的，这里我们遵循他在1845年论文中的原始推导。

Verhulst首先称，马尔萨斯的$\frac{Mdp}{pdt}=l$并不正确，他给出一个修正的增长方程。

$$
\frac{M dp}{p dt} = l - n(p - b) \tag{2}
$$

这里$M$, $l$, $n$, $b$都是常数，$p$是人口，$t$是时间。

他接着写道：d'où, en posant, pour abréger, $m = l + nb$（为了简化，设$m = l + nb$），

$$
\frac{M dp}{p dt} = m - np
$$

然后得到：

$$
dt = \frac{M dp}{mp - np^2} \tag{3}
$$

这个方程经过积分后（Cette équation étant intégrée donne），在观察到当$t = 0$时对应$p = b$的条件下，得到：

$$
t = \frac{1}{m} \log_e \left[ \frac{p(m - nb)}{b(m - np)} \right] \tag{4}
$$

这就是Verhulst在他的原始论文中推导出的积分形式的解。

从这个积分方程可以反解得到人口$p$关于时间$t$的显式表达式，这就是我们今天所知的Logistic函数。

### 传染病问题

```matlab
% 编制一个在二维网格中的小老鼠染病模拟
% M只老鼠，N只有传染病，可以通过接触传染给健康的老鼠，
% 老鼠在[0,1]x[0,1]的二维网格中随机分布，并且随机运动
% 当二者距离小于0.01时，健康的老鼠有0.1的概率被感染
M = 500; % 老鼠数量
N = 1; % 传染病数量
% 初始化老鼠位置和状态
positions = rand(M, 2); % 老鼠位置
states = zeros(M, 1); % 0: 健康, 1: 感染
infected_indices = randperm(M, N); % 随机选择N只老鼠
states(infected_indices) = 1; % 设置感染状态

% 模拟老鼠运动和传染
num_steps = 140; % 模拟步数

history_infected = zeros(num_steps, M); % 记录每一步的感染状态
history_infected(1, :) = states; % 初始状态
position_history = zeros(num_steps, M, 2); % 记录每一步的位置
position_history(1, :, :) = positions; % 初始位置

for step = 2:num_steps
    % 更新老鼠位置
    positions = positions + randn(M, 2) * 0.04; % 随机运动
    positions = mod(positions, 1); % 保持在[0,1]范围内
    
    % 检查感染传播
    for i = 1:M
        if states(i) == 0 % 如果是健康的老鼠
            % 找到所有感染的老鼠
            infected_positions = positions(states == 1, :);
            distances = sqrt(sum((infected_positions - positions(i, :)).^2, 2));
            % 如果距离小于0.01且有感染概率，则感染
            if any(distances < 0.01) && rand < 0.8
                states(i) = 1; % 感染
            end
        end
    end
    history_infected(step, :) = states; % 记录当前状态
    position_history(step, :, :) = positions; % 记录当前位置信息
end

figure;
plot(1:num_steps, sum(history_infected, 2), 'b-', 'LineWidth', 2);
xlabel('时间步');
ylabel('感染老鼠数量');
title('老鼠传染病传播模拟');
grid on;

exportgraphics(gcf, 'chp01/mouse_infection_simulation.png', 'Resolution', 300);
```

![png](/matlab/ode/from-malthus-to-logistic_files/from-malthus-to-logistic_12_0.png)

实际上，无聊的话，还可以整一个老鼠传染病传播模拟，代码如下：

```matlab
% 绘制初始状态
states = history_infected(1, :); % 获取初始状态
positions = squeeze(position_history(1, :, :)); % 获取初始位置
% 创建图形窗口
figure("Visible", 'off'); % 显示图形窗口
scatter(positions(:, 1), positions(:, 2), 50, states, 'filled');
title(['老鼠感染试验 - 时间 ', num2str(1), ' 感染老鼠数量: ', num2str(sum(states)), '/', num2str(M)]);
grid off;
box on; % 关闭网格线
axis off; % 关闭坐标轴显示
xlim([0 1]);
ylim([0 1]);
% 保存初始状态图像
fn = 'chp01/mouse_initial_distribution.gif';
if exist(fn, 'file')
    delete(fn); % 删除旧的文件
end
resolution = 100;
exportgraphics(gcf, fn , 'Resolution', resolution, 'Append', false);

for step = 2:num_steps
    states = history_infected(step, :); % 获取当前步的状态
    positions = squeeze(position_history(step, :, :)); % 获取当前步的位置
    
    % 绘制当前状态
    scatter(positions(:, 1), positions(:, 2), 50, states, 'filled');

    title(['老鼠感染试验 - 时间 ', num2str(step), ' 感染老鼠数量: ', num2str(sum(states)), '/', num2str(M)]);
    grid on;
    box on; % 关闭网格线
    axis off; % 关闭坐标轴显示
    xlim([0 1]);
    ylim([0 1]);
    
    % 保存每一步的图像
    exportgraphics(gcf, fn, 'Resolution', resolution, 'Append', true);
end

```

这个感染的过程还是挺好玩的。

![gif](/matlab/ode/from-malthus-to-logistic_files/mouse_initial_distribution.gif)

其实这个老鼠传染病传播模拟，完全是毫无意义又偏离主题的。可是既然不负责任也挺好玩，就还是留着吧。说起来，这个帖子的主题是什么来着？忘记了，好吧……

老鼠传染病的过程，同样可以用类似的微分方程来描述。

假设任意时刻，病鼠和健康鼠分别为$u$和$v$，则有：

$$
u + v = M
$$

病鼠的变化率正比于乘积$uv$，即：

$$
\frac{du}{dt} = \beta uv
$$

这里，$\beta$是病鼠和好鼠的接触概率$\times$感染概率， $\beta > 0$。可以得到方程：

$$
\frac{du}{dt} = \alpha u - \beta uv
$$

同样做不定积分可以得到：

$$
u = \frac{M}{1 +(\frac{N}{M}-1) \exp (- \alpha t)}
$$

## 常微分方程

从马尔萨斯模型到Logistic模型，可以看到利用微分的概念求解实际问题的一般过程：

1. 确定考察变量（人口、染病老鼠）；
2. 考察变量的变化规律（变化率）；
3. 列写微分方程
4. 分析初始条件、边界条件和求解条件
5. 讨论方程的解
6. 刻画解的变化规律和特征
7. 讨论解的适用条件

对于上面的微分方程，可以通过不定长积分的方式，得到包含积分常量的解，并根据初始条件确定积分常量，对于更加复杂的微分、代数方程，则需要使用数值方法求解。
