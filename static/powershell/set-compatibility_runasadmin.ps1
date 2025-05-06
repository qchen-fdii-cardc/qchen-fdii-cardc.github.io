# 设置应用程序兼容性选项的 PowerShell 脚本
param(
    [Parameter(Mandatory=$true)]
    [string]$ExePath  # 可执行文件的完整路径
    
    # [Parameter(Mandatory=$false)]
    # [switch]$RunAsAdmin = $true  # 默认以管理员身份运行
)

# 检查文件是否存在
if (-not (Test-Path $ExePath)) {
    Write-Error "指定的文件不存在: $ExePath"
    exit 1
}

# 获取文件的完整路径
$fullPath = (Get-Item $ExePath).FullName

# 构建注册表路径
$regPath = "HKCU:\Software\Microsoft\Windows NT\CurrentVersion\AppCompatFlags\Layers"

# 确保注册表项存在
if (-not (Test-Path $regPath)) {
    New-Item -Path $regPath -Force | Out-Null
}

# 设置兼容性选项
$compatibilityFlags = @()

# if ($RunAsAdmin) {
$compatibilityFlags += "RUNASADMIN" # 默认以管理员身份运行
# }

# 将标志组合成字符串
$flagsString = $compatibilityFlags -join " "

# 设置注册表值
Set-ItemProperty -Path $regPath -Name $fullPath -Value $flagsString -Type String

Write-Host "已成功设置兼容性选项:"
Write-Host "文件路径: $fullPath"
Write-Host "兼容性标志: $flagsString" 