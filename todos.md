# Emacs 配置 TODO（具体版）

## A. 配置问题审计（2026-02-15）
- [x] A1 修复 `straight.el` 与 `use-package` 集成，确保第三方包可稳定加载
  - 现象: 启动出现 `Cannot load consult/vertico/...`，`:straight` 语义不生效
- [x] A2 修复 Go 文件模式关联与 LSP 触发
  - 现象: `.go` 打开落到 `fundamental-mode`，`gopls` 不会启动
- [x] A3 修复 Rust 文件模式关联与 LSP 触发
  - 现象: `.rs` 未命中 major mode，`rust-analyzer` 不会启动
- [x] A4 统一 Zig 文件后缀关联（含 `.zig.zon`），保持 `zig-ts-mode` 优先策略
  - 现象: `.zig` 与 `.zig.zon` 路径行为不一致
- [x] A5 调整启动 warning 策略，避免把关键告警全部压制
  - 现象: 排障时关键信号被隐藏
- [x] A6 修复 `scripts/check-config.sh` 的初始化路径，避免校验结果失真
  - 现象: 脚本运行环境与真实配置目录不一致
- [ ] A7 修复 LSP 自动安装失败后的重试机制
  - 现象: 安装失败后同会话不再重试

## 0. 当前能力确认（已具备）
- [x] 启动性能优化（GC、file-name-handler、process I/O）
  - 位置: `early-init.el`
- [x] 包管理引导（`straight.el` + `use-package`）
  - 位置: `lisp/core.el`
  - 说明: 缺失包可在启动时自动安装
- [x] 基础 UI（主题、字体、行号、高亮、括号）
  - 位置: `lisp/ui.el`
- [x] 现代补全栈（vertico/orderless/marginalia/consult/embark）
  - 位置: `lisp/packages.el`
- [x] 全局快捷键（搜索/跳转/动作）
  - 位置: `lisp/keybinds.el`

## 1. 语言开发基础（LSP / 诊断 / 语法树）
- [x] 增加 `lisp/langs.el`，集中管理语言能力
- [x] 集成 LSP 客户端（`eglot`）
  - 已支持: `python`(`ty server`)、`go`(`gopls`)、`rust`(`rust-analyzer`)、`zig`(`zls`)、`c/c++`(`clangd`)
- [x] 统一诊断显示
  - `flymake` 行内/边栏策略
  - 错误导航键位（next/prev）
- [x] 启用 tree-sitter（`*-ts-mode` 优先）
  - 常见语言模式映射与自动切换
- [ ] 验收标准
  - 打开对应语言文件后，LSP 自动连接
  - `M-x flymake-show-buffer-diagnostics` 可看到诊断
  - 语法高亮由 `*-ts-mode` 生效
  - 注: 待你本机安装对应语言服务器后完成实机验收

## 2. 格式化与补全（FMT / CMP）
- [x] 增加 `lisp/editing.el`
- [x] 保存时自动格式化（优先使用 LSP formatter）
  - 已实现: `eglot-format-buffer` 优先，`apheleia` 回退
- [x] 增加补全前端（`corfu` + `cape`）
- [ ] 验收标准
  - 保存文件时格式自动应用
  - 补全菜单在编程模式可用，响应延迟可接受
  - 注: 待你本机安装/配置格式化器后完成实机验收

## 3. 项目与文件树
- [x] 文件树插件（`treemacs`）
- [x] 项目根识别与项目内切换（`project.el` + consult）
- [x] 常用键位
  - 打开/关闭文件树
  - 聚焦当前文件
- [ ] 验收标准
  - 在项目目录中可一键展示文件树并定位当前文件
  - 注: 待你本机打开项目后实机验收

## 4. 导航与搜索增强
- [x] 保留并完善现有 Consult 键位
- [x] 增加跨项目搜索与替换工作流文档（`search-workflow.md`）
  - `consult-ripgrep`
  - `embark` 批量动作
- [ ] 验收标准
  - 能在 2~3 次按键内完成“搜索 -> 跳转 -> 批处理”
  - 注: 待你本机按文档流程实机验收

## 5. 配置结构与健壮性
- [x] 在 `init.el` 启用模块加载顺序说明（注释）
- [x] 所有模块统一 `provide/require` 命名规范
- [x] 增加缺失依赖的降级策略
  - 已实现: 无网络/缺依赖时不阻断启动，仅给出 warning
- [ ] 验收标准
  - `emacs --batch -Q -l early-init.el -l init.el` 无致命错误

## 6. 文档与自检
- [x] 新增 `README.org` 或 `README.md`
  - 说明系统依赖（ripgrep、字体、语言服务器）
  - 首次启动会自动装包
- [x] 新增 `scripts/check-config.sh`
  - 批处理加载检查
  - 可选字节编译检查
- [ ] 验收标准
  - 新机器按文档 10 分钟内可完成可用环境
  - 注: 待新机器环境实机验收

## 7. 建议实施顺序
1. `langs.el`（LSP + 诊断 + tree-sitter）
2. `editing.el`（格式化 + 补全）
3. 文件树与项目导航
4. 文档与自检脚本
