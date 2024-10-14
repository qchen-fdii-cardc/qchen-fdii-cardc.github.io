# MATLAB中的文件读写

这一篇就要简单介绍MATLAB中的典型文件类型和文件操作。

## 基于字节流的接口


Matlab本身提供的文件操作是比较接近底层的，这一套底层的文件原语，主要是`fopen`、`fclose`、`fread`、`fwrite`、`fseek`、`ftell`、`feof`、`ferror`等函数。这些函数的使用方法和C语言中的文件操作函数类似，具体的用法可以参考MATLAB的官方文档。


### 字节流
操作系统的文件操作，一般区分二进制和文本文件，在Matlab中，采取是数组的形式进行读写，所以在Matlab中，可以设定非常灵活的读写方式。


当需要读入文件时，根据表达文件的字符的不同，可以在调用`fread`时，直接制定数组元素的数据类型，比如`char`、`int8`、`int16`、`int32`、`int64`、`uint8`、`uint16`、`uint32`、`uint64`、`single`、`double`等。这样就可以直接读取文件的内容。

当然，这里还会涉及到文件编码的问题，在这个入门的教程中，就不展开讨论了。


```matlab
help fopen
```

     fopen - 打开文件或获得有关打开文件的信息
        此 MATLAB 函数 打开文件 filename 以便以二进制读取形式进行访问，并返回等于或大于 3
        的整数文件标识符。MATLAB 保留文件标识符 0、1 和 2 分别用于标准输入、标准输出（屏
        幕）和标准错误。
    
        语法
          fileID = fopen(filename)
          fileID = fopen(filename,permission)
          fileID = fopen(filename,permission,machinefmt,encodingIn)
          [fileID,errmsg] = fopen(___)
    
          fIDs = fopen('all')
    
          filename = fopen(fileID)
          [filename,permission,machinefmt,encodingOut] = fopen(fileID)
    
        输入参数
          filename - 要打开的文件的名称
            字符向量或字符串标量
          permission - 文件访问类型
            'r' (默认值) | 'w' | 'a' | 'r+' | 'w+' | 'a+' | 'A' | 'W'
          machinefmt - 读取或写入字节或位的顺序
            'n' (默认值) | 'b' | 'l' | 's' | 'a'
          encodingIn - 字符编码
            'UTF-8' | 'ISO-8859-1' | 'windows-1251' | 'windows-1252'
          fileID - 已打开文件的文件标识符
            整数
    
        另请参阅 fclose, ferror, fseek, ftell, feof, fscanf, fprintf, fread,
          fwrite, frewind
    
        已在 R2006a 之前的 MATLAB 中引入
        fopen 的文档
           doc fopen
    
        fopen 的其他用法
    
           i2c/fopen    icinterface/fopen    instrument/fopen    serial/fopen
    
    


```matlab
help fwrite
```

     fwrite - 将数据写入二进制文件
        此 MATLAB 函数 将数组 A 的元素按列顺序以 8 位无符号整数的形式写入一个二进制文件。该二
        进制文件由文件标识符 fileID 指示。使用 fopen 可打开文件并获取 fileID 值。完成写入
        后，请调用 fclose(fileID) 来关闭文件。
    
        语法
          fwrite(fileID,A)
          fwrite(fileID,A,precision)
          fwrite(fileID,A,precision,skip)
          fwrite(fileID,A,precision,skip,machinefmt)
    
          count = fwrite(___)
    
        输入参数
          fileID - 文件标识符
            整数 | 1 | 2
          A - 要写入的数据
            数值数组 | 字符数组 | 字符串数组
          precision - 要写入的值的类和大小
            'uint8' (默认值) | 字符向量 | 字符串标量
          skip - 要跳过的字节数
            0 (默认值) | 标量
          machinefmt - 字节写入顺序
            'n' (默认值) | 'b' | 'l' | 's' | 'a'
    
        示例
          openExample('matlab/WriteUint8DataToFileExample')
          openExample('matlab/Write4byteIntegersToFileExample')
          openExample('matlab/AppendDataToBinaryFileExample')
          openExample('matlab/WriteBinaryFileWithBigendianByteOrderingExample')
    
        另请参阅 fclose, ferror, fopen, fprintf, fread, fscanf, fseek, ftell
    
        已在 R2006a 之前的 MATLAB 中引入
        fwrite 的文档
           doc fwrite
    
        fwrite 的其他用法
    
           i2c/fwrite            instrument/fwrite    serial/fwrite
           icinterface/fwrite
    
    


```matlab
help fread
```

     fread - 读取二进制文件中的数据
        此 MATLAB 函数 将打开的二进制文件中的数据读取到列向量 A 中，并将文件指针定位在文件结尾
        标记处。该二进制文件由文件标识符 fileID 指示。使用 fopen 可打开文件并获取 fileID
        值。读取文件后，请调用 fclose(fileID) 来关闭文件。
    
        语法
          A = fread(fileID)
          A = fread(fileID,sizeA)
          A = fread(fileID,precision)
          A = fread(fileID,sizeA,precision)
          A = fread(___,skip)
          A = fread(___,machinefmt)
          [A,count] = fread(___)
    
        输入参数
          fileID - 文件标识符
            整数
          sizeA - 输出数组的维度
            Inf (默认值) | 整数 | 二元素行向量
          precision - 要读取的值的类和大小
            'uint8=>double' (默认值) | 字符向量或字符串标量
          skip - 要跳过的字节数
            0 (默认值) | 标量
          machinefmt - 读取字节的顺序
            'n' (默认值) | 'b' | 'l' | 's' | 'a'
    
        输出参数
          A - 文件数据
            列向量 | 矩阵
          count - 读取的字符数
            标量
    
        示例
          openExample('matlab/ReadEntireFileofuint8DataExample')
          openExample('matlab/ReadEntireFileofDoublePrecisionDataExample')
          openExample('matlab/ReadSelectedRowsorColumnsfromFileExample')
          openExample('matlab/ReadDigitsofBinaryCodedDecimalValuesExample')
    
        另请参阅 fclose, fgetl, fopen, fscanf, fprintf, fwrite, fseek, ftell
    
        已在 R2006a 之前的 MATLAB 中引入
        fread 的文档
           doc fread
    
        fread 的其他用法
    
           i2c/fread            instrument/fread    serial/fread    udp/fread
           icinterface/fread
    
    

### 读写示例

在Matlab中，向文件写入的都是数组（Matrix），前面已经说过，数组的元素类型也是数组的类型。那么这里可以写入和读取的数据类型就是Matlab中的数据类型。这也是这个比较接近底层的文件操作依然在Matlab中有用武之地的原因。

在下面的例子中，我们在文件中写入一个字符串，然后再读取出来。可以看到很有意思的是，我们把字符串作为一个`char`的数组写入文件中。在读出的时候，可以选择读取`char`的数组，也可以选择读取为`uint8`的数组，还能够不同的数据类型，根据数据类型的实际大小，读取为不同长度的数组。

下面的例子中，可以看到，字符`char`和`uint8`为1字节，`int`为4字节，`double`为8字节。

根据这样的方式，如果我们仔细设定数据的类型和字节读写顺序，就能够实现非常灵活又高效的二进制文件。通常我们如果需要读写别的软件、硬件系统产生的字节流数据，都是采用这种方式。实际上Matlab中与硬件设备的接口（包括网络接口、RS232等），也是通过这种方式实现的。这些硬件的接口函数中，通常也会用类似打开文件的方式得到句柄，并利用这些句柄进行读写操作。


```matlab
fid = fopen('data.txt','w');
data = 'Hello, the World!';
fwrite(fid, data, 'char');
fclose(fid);

dispVector('data.txt', 'char', '%c ');
dispVector('data.txt', 'char', '%d ');

dispVector('data.txt', 'uint8', '%c ');
dispVector('data.txt', 'uint8', '%d ');

dispVector('data.txt', 'int', '%d ');
dispVector('data.txt', 'single', '%g ');
dispVector('data.txt', 'double', '%g ');


function vecRead = readVector(filename, type)
    fid = fopen(filename, 'r');
    vecRead = fread(fid, type);
    fclose(fid);
end

function dispVector(filename, type, formatSpec)
    v = readVector(filename, type);
    fprintf('[');
    fprintf(formatSpec, v);
    fprintf(']\n');
end
```

    [H e l l o ,   t h e   W o r l d ! ]
    [72 101 108 108 111 44 32 116 104 101 32 87 111 114 108 100 33 ]
    [H e l l o ,   t h e   W o r l d ! ]
    [72 101 108 108 111 44 32 116 104 101 32 87 111 114 108 100 33 ]
    [1819043144 1948265583 1461740904 1684828783 ]
    [1.14314e+27 5.0761e+31 1.76357e+14 1.74467e+22 ]
    [2.31597e+251 5.62865e+175 ]
    

## 用文本表示数据

我们有几种读取这类文本数据的方式：

1. 数组格式化读入
   1. `fscanf`函数，这个函数的使用方法和C语言中的`fscanf`函数类似，但是Matlab中的`fscanf`函数更加灵活，可以读取不同的数据类型，也可以读取不同的数据格式。
2. 行为基础的读入
   1. `fgetl`和`fgets`函数，这两个函数用来读取一行文本，`fgetl`删除换行符，`fgets`保留换行符。这两个函数返回的是一个字符串，通常可以通过`str2num`函数将字符串转换为数值。
   2. `textread`函数，这个函数是一个比较古老的函数，用来读取文本文件，在被`textscan`所替代。
   3. `textscan`函数，这个函数是一个比较新的函数，用来读取文本文件，可以指定不同的数据类型，也可以指定不同的数据格式。

### 数组格式化读写

`fscanf`函数的使用方法和C语言中的`fscanf`函数类似，但是Matlab中的`fscanf`函数更加灵活，可以读取不同的数据类型，也可以读取不同的数据格式。

这个函数跟`fprintf`函数是一对，`fprintf`函数用来写入数据，`fscanf`函数用来读取数据。

这两个函数与其C语言对应版本还是有挺大区别，Matlab中说到一个类型，实际上都是指某个类型的数组，所有这两个函数，都可以直接操作数组。

首先看这个例子：


```matlab
a = magic(3);
disp(a);

fid = fopen('data.txt','w');
fprintf(fid, '%d ', a);
fclose(fid);

% show text file content
type data.txt

% 默认的读入，得到一个列向量
fid = fopen('data.txt','r');
b = fscanf(fid, '%d');
fclose(fid);
disp(b);

% 按照矩阵的形式读入，按照列先的方式转化为矩阵
fid = fopen('data.txt','r');
b = fscanf(fid, '%d', [3,3]);
fclose(fid);
disp(b);
```

         8     1     6
         3     5     7
         4     9     2
    
    
    8 3 4 1 5 9 6 7 2 
         8
         3
         4
         1
         5
         9
         6
         7
         2
    
         8     1
         3     5
         4     9
    
    

注意，`fprintf`函数会将其中的`formatSpec`参数中的`%s`、`%d`、`%f`等格式符号替换为对应的数据，然后写入文件。当不设定`sizeA`参数时，`fprintf`函数会将整个数组写入文件。并且，写入的方式是按照列的方式写入的。此处的`%d `的这个空格，也是的输出的数据之间有空格。

在使用`fscanf`函数时，`formatSpec`参数中的`%s`、`%d`、`%f`等格式符号，会被替换为对应的数据，然后读取文件中的数据。当不设定`sizeA`参数时，`fscanf`函数会读取整个文件的数据。并且，读取的方式是按照列的方式读取的。这个例子中，`fscanf`函数读取的数据是一个列向量。当设定`sizeA`参数时，`fscanf`函数会读取指定大小的数据。这个例子中，`fscanf`函数读取的数据是一个3x3的矩阵。

这个例子中的`type`函数跟命令行中的`type`命令类似，用来显示文件的内容。


```matlab
help fprintf
```

     fprintf - 将数据写入文本文件
        此 MATLAB 函数 按列顺序将 formatSpec 应用于数组 A1,...An 的所有元素，并将数据写入
        到一个文本文件。fprintf 使用在对 fopen 的调用中指定的编码方案。
    
        语法
          fprintf(fileID,formatSpec,A1,...,An)
          fprintf(formatSpec,A1,...,An)
    
          nbytes = fprintf(___)
    
        输入参数
          fileID - 文件标识符
            1 (默认值) | 2 | 标量
          formatSpec - 输出字段的格式
            格式化操作符
          A1,...,An - 数值数组或字符数组
            标量 | 向量 | 矩阵 | 多维数组
    
        输出参数
          nbytes - 字节数
            标量
    
        示例
          openExample('matlab/PrintLiteralTextandArrayValuesExample')
          openExample('matlab/PrintDoublePrecisionValuesasIntegersExample')
          openExample('matlab/GetNumberofBytesWrittentoFileExample')
    
        另请参阅 disp, fclose, ferror, fopen, fread, fscanf, fwrite, fseek,
          ftell, sprintf
    
        已在 R2006a 之前的 MATLAB 中引入
        fprintf 的文档
           doc fprintf
    
        fprintf 的其他用法
    
           dlarray/fprintf     icinterface/fprintf    simulink/fprintf
           gpuArray/fprintf    serial/fprintf
    
    


```matlab
help fscanf
```

     fscanf - 读取文本文件中的数据
        此 MATLAB 函数 将打开的文本文件中的数据读取到列向量 A 中，并根据 formatSpec 指定的
        格式解释文件中的值。fscanf 函数在整个文件中重新应用该格式，并将文件指针定位在文件结尾标
        记处。如果 fscanf 无法将 formatSpec 与数据相匹配，将只读取匹配的部分并停止处理。
    
        语法
          A = fscanf(fileID,formatSpec)
          A = fscanf(fileID,formatSpec,sizeA)
          [A,count] = fscanf(___)
    
        输入参数
          fileID - 文件标识符
            整数
          formatSpec - 数据字段的格式
            字符向量 | 字符串标量
          sizeA - 输出数组的维度
            Inf (默认值) | 整数 | 二元素行向量
    
        输出参数
          A - 文件数据
            列向量 | 矩阵 | 字符向量 | 字符数组
          count - 读取的字符数
            标量
    
        示例
          openExample('matlab/ReadFileContentsIntoColumnVectorExample')
          openExample('matlab/ReadFileContentsIntoArrayExample')
    
        另请参阅 fopen, fprintf, textscan, sscanf, fgetl, fgets, fread
    
        已在 R2006a 之前的 MATLAB 中引入
        fscanf 的文档
           doc fscanf
    
        fscanf 的其他用法
    
           icinterface/fscanf    serial/fscanf    udp/fscanf
    
    

### 基于行的读写

`fgetl`/`fgets`针对的都是单行的数据，每次读入一行，这一行都被当做一个字符串处理。这种方式适合处理文本文件，比如配置文件、日志文件等。

通常会在读取一行之后，会自行做分割（`split`函数），再用`str2num`函数将字符串转换为数值。这个函数的使用方法和`str2double`函数类似，只是`str2num`函数可以处理更多的数据类型。

当然Matlab提供了更为强大的`textscan`函数，这个函数可以指定不同的数据类型，也可以指定不同的数据格式。之前（R2006a）我们使用的是`textread`函数。

这两个函数逐行解析文本文件，并根据指定的格式解析数据。

在实际的应用中，我们通常会使用更加特化的存储格式，这些方式通常会更加流行，并且在不同的软件系统中都得到了相应的支持。所以我们这里就不再赘述这几个有一定难度的函数，如果感兴趣可以通过`doc`函数查看这几个函数的帮助文档。

## 几种特殊的流行格式
实际上，在Matlab中，使用得最多的还是下面几种格式的文件：

1. 二进制文件
   1. mat文件
   2. fig文件
   3. xls文件
2. 文本文件
   1. csv文件
   
其他格式的文件，例如音频、视频、图像等，Matlab也有相应的函数支持，还有json、xml等格式的文本文件，也不在这个入门教程的范围内。

### mat文件

mat文件是Matlab的专有文件格式，这个文件格式是一个二进制文件，可以保存Matlab的变量，包括数组、结构体、函数句柄等。这个文件格式是Matlab的专有格式。

Matlab提供了`load`和`save`函数，可以读写这个文件格式。

这个文件格式的优点是内在支持，可用性较好、前向兼容性也处理得不错，读写效率也比较高。在计算中要保存中间结果，通常会采用这个格式。

### fig文件

fig文件是Matlab的图形文件，这个文件格式是一个二进制文件，可以保存Matlab的图形，包括图形的数据、图形的设置、图形的样式等。这个文件格式是Matlab的专有格式。

Matlab提供了`openfig`和`savefig`函数，可以读写这个文件格式。

这个格式的文件读出的图像，支持用鼠标进行交互，可以进行编辑，也可以进行保存为其他格式的文件。还能够通过`findobj`函数找到图形中的对象，进行进一步的操作。

### xls文件

xls文件是Excel的文件格式，这个文件格式是一个二进制文件，可以保存Excel的数据，包括表格、图表、宏等。这个文件格式是Excel的专有格式。

Matlab提供了`xlsread`和`xlswrite`函数，可以读写这个文件格式。

### csv文件

csv文件是文本文件的一种，这个文件格式是一个文本文件，可以保存表格数据，每行数据用逗号分隔，每行数据用换行符分隔。

Matlab提供了`csvread`和`csvwrite`函数，可以读写这个文件格式。

## 新的数据读写方式

在R2019b版本中，Matlab引入了新的数据读写方式，这个方式是基于`readtable`和`writetable`函数的。这个函数的优点是更加灵活，可以读写不同的数据类型，也可以读写不同的数据格式。

这两个函数的使用方法和`textscan`函数类似，但是更加灵活，可以读写不同的数据类型，也可以读写不同的数据格式。

此外，Matlab还引入了`readmatrix`和`writematrix`函数，这两个函数是`readtable`和`writetable`函数的简化版本，只能读写数值数据，但是更加高效。


## 总结

1. Matlab中实现了一个接近底层的文件操作原语，主要是`fopen`、`fclose`、`fread`、`fwrite`、`fseek`、`ftell`、`feof`、`ferror`等函数。
2. 这个文件操作原语也具备Matlab直接操作数组的特性，因此可以实现非常灵活又高效的二进制文件读写。
3. 针对文本文件，Matlab提供了`fscanf`、`fgetl`、`fgets`、`textread`、`textscan`等函数，可以实现文本文件的读写。
4. 针对流行的文件格式，Matlab提供了`load`、`save`、`openfig`、`savefig`、`xlsread`、`xlswrite`、`csvread`、`csvwrite`等函数，可以实现这些文件格式的读写。
5. 新的数据文件读写倾向于采用同一个接口来针对不同文件，例如`readtable`和`writetable`函数，`readmatrix`和`writematrix`函数。

