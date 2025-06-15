# ime.ps1

param(
    # 引数として "on", "off", "toggle" のいずれかを受け取る。
    # 省略した場合は "toggle" として動作する。
    [ValidateSet("on", "off", "toggle", "")]
    [string]$State = "toggle"
)

# Win32 APIを呼び出すためのC#コードを定義
$csharpSource = @"
using System;
using System.Runtime.InteropServices;

public static class IME
{
    // Win32 APIのインポート
    [DllImport("user32.dll")]
    public static extern IntPtr GetForegroundWindow();

    [DllImport("imm32.dll")]
    public static extern IntPtr ImmGetDefaultIMEWnd(IntPtr hWnd);

    [DllImport("user32.dll")]
    public static extern IntPtr SendMessage(IntPtr hWnd, uint Msg, IntPtr wParam, IntPtr lParam);

    // 送信するメッセージとパラメータの定数
    public const uint WM_IME_CONTROL = 0x283;
    public const int IMC_GETOPENSTATUS = 5;
    public const int IMC_SETOPENSTATUS = 6;
}
"@

# C#コードをPowerShellに読み込む
Add-Type -TypeDefinition $csharpSource -Language CSharp

# 現在アクティブなウィンドウのハンドルを取得
$hwnd = [IME]::GetForegroundWindow()
if ($hwnd -eq [IntPtr]::Zero) {
    # アクティブなウィンドウがなければ何もしない
    return
}

# 対応するIMEウィンドウのハンドルを取得
$imeWnd = [IME]::ImmGetDefaultIMEWnd($hwnd)
if ($imeWnd -eq [IntPtr]::Zero) {
    # IMEを持たないアプリケーション（メモ帳など）の場合は何もしない
    return
}

# 現在のIMEの状態を取得 (ONなら$true, OFFなら$false)
$isImeOn = ([IME]::SendMessage($imeWnd, [IME]::WM_IME_CONTROL, [IntPtr]::new([IME]::IMC_GETOPENSTATUS), [IntPtr]::Zero) -ne [IntPtr]::Zero)

# 設定したい新しい状態を決定
$newState = $null
switch ($State.ToLower()) {
    "on"     { $newState = $true }
    "off"    { $newState = $false }
    "toggle" { $newState = -not $isImeOn }
}

# IMEの状態を更新
if ($newState -ne $null) {
    $lParam = if ($newState) { [IntPtr]1 } else { [IntPtr]0 }
    [IME]::SendMessage($imeWnd, [IME]::WM_IME_CONTROL, [IntPtr]::new([IME]::IMC_SETOPENSTATUS), $lParam)
}