+++
title = 'SettingsGroup_in_Matlab图形界面的设置选项'
date = 2024-11-23T08:44:22+08:00
draft = false
mathjax = false
categories = ['matlab']
tags = ['matlab', 'tutorial', 'settings']
toc = true
tocBorder = true
+++



    只要你知道你自己正在做什么，那么你怎么做都行。—— C.J. Date

## Matlab的界面与设置

### Matlab的界面

Matlab的界面是GUI设计中非常值得讨论的一个议题。先来看，默认的Matlab界面。

![](/matlab-img/matlab-gui.png)

这里的界面从上到下分为了四个部分，分别是：

- 工具栏标签，有几个标签，都可以选择展开不同的工具栏。
- 工具栏，包括了一些常用的工具，可以通过按钮（带文字标签）来访问各项功能
- 文件区
  - 文件夹导航栏，可以用来快速访问文件夹和文件夹内的文件
  - 文件编辑器，可以用来编辑文件，左边导航栏点击文件、命令行`edit filename`、右键文件选择`Open`都可以打开文件编辑器
- 细节与命令行
  - 当前选定文件的细节信息
  - 命令行，可以用来输入命令，也可以用来查看输出
- 最右边有一个显示当前工作区变量的场口（5），高度是两栏

这个界面是可以调整的，拖动相应的边框可以调整大小，也可以通过工具栏`Home`-`Layout`来选择预设的布局。上面这个`Home`-`Layout`的描述就是标签-工具按钮的形式，可以通过点击标签来展开工具栏，并在展开的工作栏上选择工具按钮。当然中文界面和英文界面略有却别，但是按钮的图标是一致的。比如这个`Layout`（布局）就在这个位置。

![](/matlab-img/layout-button.png)

切换不同的语言版本，也可以大概通过位置和图标来猜测具体的功能。

### 设置

在Matlab中，不但可以设置整个界面的布局，各个布局也还有很多细节可以设置。

在上面的1、2、3、4、5这几个区域的右上角，都有一个小小的三角形，点击这个三角形，可以展开一个菜单，里面有一些设置选项。比如在文件编辑器的右上角，点击这个三角形，可以看到这样的菜单。

![](/matlab-img/dockable.png)

这里提供了最小化、最大化、浮动、停靠、锁定当前文件等操作。在文件夹导航栏的右上角，也有类似的设置和一些针对文件夹的快速操作。

另外，在工具栏`Home`-`Preferences`中，也可以打开一个设置窗口，里面有很多设置选项，可以设置Matlab的界面和GUI的各种行为。

![](/matlab-img/preference.png)

这个按钮可以打开一个设置窗口，里面有很多设置选项，可以设置Matlab的界面和GUI的各种行为。


![](/matlab-img/选择语言.png)

首先，我们可以看看Matlab的语言选择，这个选项在Matlab-General下面的最后一个面板，Desktop Language，可以选择Chinese和Engish。此外，Matlab-Help下面还有一个首选语言的选择，也可以选择中文。

![](/matlab-img/help-language.png)


## 命令行窗口设置

这里的`Command Window`，可以设置命令行的各个特性。

![](/matlab-img/command-window-settings.png)

对应中文就是这样：

![](/matlab-img/command-window-settings-cn.png)
### 设置项目


简单来看，这里可以设置的有：

- 文本显示
  - 数字显示格式
  - 是否显示额外的空行
- 日期格式
- 显示
  - 是否自动折行
  - 是否把举证显示的宽度设为80列
  - 是否显示提示
  - 是否显示链接
  - 是否提示错误输入的函数名和变量名
  - 命令行窗口的缓冲区大小
- Tab的大小

### 文本显示

文本显示的两个选项，一个是数字显示格式，一个是是否显示额外的空行。

这两个功能还有一个命令可以直接访问，`format`。

```matlab
format short
format long
format short e
format long e
```

这就能够调整数字的显示格式。请务必尝试一下，看看有什么不同。

关于显示额外的行，命令行窗口的显示是可以设置的，可以通过`format compact`和`format loose`来设置，跟这里的选项对应。

### 显示（Display）

第一个选项就是自动折行，在命令行中输入一个很长的语句，如果选择了自动折行，那么就会自动折行显示，否则就会横向滚动显示。

第二选项就是显示的宽度，如果矩阵显示的列宽超过80列，就会自动整体换行显示。

```matlab
>> magic(15)
ans =
  列 1 至 13
   122   139   156   173   190   207   224     1    18    35    52    69    86
   138   155   172   189   206   223    15    17    34    51    68    85   102
   154   171   188   205   222    14    16    33    50    67    84   101   118
   170   187   204   221    13    30    32    49    66    83   100   117   134
   186   203   220    12    29    31    48    65    82    99   116   133   150
   202   219    11    28    45    47    64    81    98   115   132   149   151
   218    10    27    44    46    63    80    97   114   131   148   165   167
     9    26    43    60    62    79    96   113   130   147   164   166   183
    25    42    59    61    78    95   112   129   146   163   180   182   199
    41    58    75    77    94   111   128   145   162   179   181   198   215
    57    74    76    93   110   127   144   161   178   195   197   214     6
    73    90    92   109   126   143   160   177   194   196   213     5    22
    89    91   108   125   142   159   176   193   210   212     4    21    38
   105   107   124   141   158   175   192   209   211     3    20    37    54
   106   123   140   157   174   191   208   225     2    19    36    53    70
  列 14 至 15
   103   120
   119   121
   135   137
   136   153
   152   169
   168   185
   184   201
   200   217
   216     8
     7    24
    23    40
    39    56
    55    72
    71    88
    87   104
```

这样实际上是比较美观的。如果关掉这个80列的选项，就会利用命令行窗口的所有列来显示，如果还是不够显示，就是进行上面这样的标记列范围的换行显示。

到这里，Matlab就没有什么秘密了，你可以自己调整Matlab的界面和行为的选项，让它更符合你的使用习惯。

## 话说这一节是怎么冒出来的？

    兔洞像隧道一样笔直地延伸，
    然后突然陡然向下倾斜，
    来得太突然，
    爱丽丝来不及考虑要停住自己，
    就发现自己正在跌落一个非常深的井中。

跟计算机玩耍与跟人玩耍最不一样的就是，一般的人的反应很快就会被穷尽，而计算机就不一样，我们很难知道我们会停在哪里。或者反过来也是一样成立的，对于某些人来说……

当写完上面的80行迷之数字设置时，我突然想到，这里的`format long g`/`format compact`在哪里？稍微搜索了一下帮助文件就发现，原来，还有个`SettingsGroup`的东西，可以用来设置这些东西。

### SettingsGroup

```matlab
>> settings
ans = 
  SettingsGroup - 属性:
    roadrunner: [1×1 SettingsGroup]
       driving: [1×1 SettingsGroup]
      Simulink: [1×1 SettingsGroup]
     slhistory: [1×1 SettingsGroup]
        matlab: [1×1 SettingsGroup]
```

啊哈，一下子暴漏了啊。

这里的`SettingsGroup`是一个类，可以用来设置Matlab的各种属性。比如`matlab`这个属性，就是用来设置Matlab的属性的。刚好对应与前面图形中的Matlab。那我们就来看看这个`matlab`的属性。

```matlab
>> settings().matlab
ans = 
  SettingsGroup 'matlab' - 属性:
    toolboxpathcache: [1×1 SettingsGroup]
            keyboard: [1×1 SettingsGroup]
         appdesigner: [1×1 SettingsGroup]
       commandwindow: [1×1 SettingsGroup]
               fonts: [1×1 SettingsGroup]
              colors: [1×1 SettingsGroup]
        codeanalyzer: [1×1 SettingsGroup]
             general: [1×1 SettingsGroup]
              editor: [1×1 SettingsGroup]
```
看到这里，我们不得不把`settings`后面的括号加上。所以我们也可以创建一个变量来把这个结果存在workspace中。

```matlab
>> s = settings;
>> s.matlab.commandwindow
ans = 
  SettingsGroup 'matlab.commandwindow' - 属性:
             DisplayLineSpacing: [1×1 Setting]
                  NumericFormat: [1×1 Setting]
    UseEightyColumnDisplayWidth: [1×1 Setting]
                    suggestions: [1×1 SettingsGroup]
```

我们在前面设置的`DisplayLineSpacing`、`UseEightyColumnDisplayWidth`和`NumericFormat`就在这里了。这里的`Setting`是一个类，可以用来设置具体的属性。

```matlab
>> s.matlab.commandwindow.DisplayLineSpacing
ans = 
  Setting 'matlab.commandwindow.DisplayLineSpacing' - 属性:
          ActiveValue: 'compact'
       TemporaryValue: <no value>
        PersonalValue: 'compact'
    InstallationValue: <no value>
         FactoryValue: 'loose'
```

这里唯一注意的就是，这个简单的`Setting`还挺烦人的，有`ActiveValue`、`TemporaryValue`、`PersonalValue`、`InstallationValue`、`FactoryValue`这么多值，这个`ActiveValue`是当前生效的值，`PersonalValue`是用户设置的值，`InstallationValue`是安装时的值，`FactoryValue`是默认值。这里的`TemporaryValue`是临时设置的值，如果设置了这个值，那么这个值会覆盖`ActiveValue`，但是只在这个会话中有效。


如果，我们只是要玩下票，下次不来了，就这是这个`TemporaryValue`，这样就不会影响到下次的使用。如果我们要永久改变，就设置`PersonalValue`，这样就会永久改变这个值。


## 结束语

    “在这儿！”爱丽丝喊道。
