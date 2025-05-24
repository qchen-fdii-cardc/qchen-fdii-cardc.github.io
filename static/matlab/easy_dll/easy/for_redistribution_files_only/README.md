# MATLAB DLL 调用示例

这是一个使用CMake构建的C语言项目，用于演示如何调用MATLAB生成的DLL。

## 项目结构

- `easy.dll` - MATLAB生成的动态链接库
- `easy.lib` - 导入库文件
- `easy.h` - 头文件
- `main.c` - 主程序文件
- `CMakeLists.txt` - CMake配置文件

## 构建说明

1. 确保已安装CMake（3.10或更高版本）
2. 创建构建目录：
   ```bash
   mkdir build
   cd build
   ```
3. 配置项目：
   ```bash
   cmake ..
   ```
4. 构建项目：
   ```bash
   cmake --build .
   ```

## 运行程序

构建完成后，可执行文件将位于build目录下。直接运行可执行文件即可：

```bash
./matlab_dll_demo
```

## 注意事项

- 确保MATLAB运行时环境已正确安装
- 运行时需要确保`easy.dll`在可执行文件的同一目录下 