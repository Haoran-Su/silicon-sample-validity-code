# LLM Experiment Runner / LLM 实验批处理工具

English | [中文](#中文文档)

---

## Table of Contents (English)

- [1. Overview](#1-overview)
- [2. Features](#2-features)
- [3. UI Options](#3-ui-options)
- [4. Prompt Construction](#4-prompt-construction)
- [5. Output Parsing](#5-output-parsing)
- [6. Input Excel Requirements](#6-input-excel-requirements)
- [7. Output Excel Columns](#7-output-excel-columns)
- [8. Installation](#8-installation)
- [9. Quick Start](#9-quick-start)
- [10. Notes](#10-notes)

---

## 1. Overview

LLM Experiment Runner is a Tkinter desktop tool for batch experiments using Excel rows as samples/personas.
It supports OpenRouter/OpenAI/SiliconCloud, parses <x> results, optionally captures reasoning text, and exports all results to Excel.

## 2. Features

- Multi-provider API support
- Per-provider API key persistence
- Scrollable UI
- Prompt builder (prefix + auto persona + suffix)
- Live current-sample preview
- <x> extraction (Bracket_Result)
- Final decision parsing (Final_Decision)
- Optional separator-based split
- Optional reasoning capture (Reasoning_Content)
- Token/cost tracking
- Stop button and retry-failed mode

## 3. UI Options

- Select Excel File: input .xlsx
- Select Output Folder: where result file is saved
- Provider / API Key / Model
- Retry failed rows only
- Generation parameters:
  - temperature
  - top_p
  - max_tokens
  - timeout_sec
  - retries
  - interval_sec
  - separator
- Prompt Builder:
  - Prefix text
  - Suffix text
- Current Sample Preview
- Start / Stop
- Run Log

## 4. Prompt Construction

For each row:
1) Build middle string from non-result columns:
   your {column} is {value}
2) Join by full-width semicolon + space: ；
3) Final prompt = Prefix + middle + Suffix (with blank lines between blocks)

## 5. Output Parsing

- Raw_Output: full model response
- Split by separator into:
  - Parsed_Reason
  - Parsed_Result
- Extract first <...> to Bracket_Result
- Final_Decision priority:
  1) parsed from <x>
  2) fallback JSON {"decision": ...} if available
- Reasoning_Content: best-effort extraction when provider returns reasoning fields

## 6. Input Excel Requirements

- .xlsx format
- First row as headers
- One row = one sample
- Any number of columns is supported

## 7. Output Excel Columns

- Original input columns (kept)
- Raw_Output
- Reasoning_Content
- Parsed_Reason
- Parsed_Result
- Bracket_Result
- Final_Decision
- Prompt_Tokens
- Completion_Tokens
- Total_Tokens
- Estimated_Cost_USD
- Status

## 8. Installation

pip install pandas requests openpyxl

Run:
python runner.py

## 9. Quick Start

1. Select Excel file
2. Select output folder
3. Choose provider/model and fill API key
4. Set prompt prefix/suffix
5. Ask model to output in format like: abc...<x>
6. Click Start

## 10. Notes

- Cost is estimated using local price table
- Reasoning capture depends on provider/model response schema
- Stop is graceful: current row finishes, then loop stops

---

# 中文文档

[English](#table-of-contents-english) | 中文

## 目录（中文）

- [1. 工具简介](#1-工具简介)
- [2. 功能列表](#2-功能列表)
- [3. 界面选项说明](#3-界面选项说明)
- [4. Prompt 拼接逻辑](#4-prompt-拼接逻辑)
- [5. 输出解析逻辑](#5-输出解析逻辑)
- [6. 输入 Excel 要求](#6-输入-excel-要求)
- [7. 输出 Excel 列说明](#7-输出-excel-列说明)
- [8. 安装方式](#8-安装方式)
- [9. 快速使用步骤](#9-快速使用步骤)
- [10. 注意事项](#10-注意事项)

---

## 1. 工具简介

LLM Experiment Runner 是一个基于 Tkinter 的桌面批处理工具。
它把 Excel 每一行当作一个样本/人物画像，批量调用大模型接口，并将结果写回 Excel。

支持：
- OpenRouter
- OpenAI
- SiliconCloud

## 2. 功能列表

- 多平台 API 调用
- 每个平台独立保存 API Key
- 可滚动界面（避免内容显示不全）
- Prompt 三段式构造（前置文本 + 自动变量串 + 后置文本）
- 当前样本实时预览
- <x> 结果提取
- 最终决策值提取
- 分隔符拆分理由/结果
- 深度思考文本抓取（若接口返回）
- Token 与成本估算
- 中途 Stop 停止
- 失败样本重跑模式

## 3. 界面选项说明

- Select Excel File：选择输入数据文件
- Select Output Folder：选择结果导出目录
- Provider / API Key / Model：选择平台、密钥、模型
- Retry failed rows only：仅重跑失败样本
- 参数区：
  - temperature：随机性
  - top_p：采样阈值
  - max_tokens：最大输出长度
  - timeout_sec：单次请求超时
  - retries：失败重试次数
  - interval_sec：样本间隔秒数
  - separator：输出拆分分隔符
- Prompt Builder：
  - Prefix text：前置文本
  - Suffix text：后置文本
- Current Sample Preview：查看当前发送的完整 prompt
- Start / Stop：开始/停止
- Run Log：运行日志

## 4. Prompt 拼接逻辑

每条样本会自动生成中间变量串：
your {列名} is {值}； your {列名2} is {值2} ...

然后按三段拼接：
1) 前置文本（你填写）
2) 自动变量串（程序生成）
3) 后置文本（你填写）

## 5. 输出解析逻辑

- Raw_Output：模型原始返回
- 按 separator 拆分为：
  - Parsed_Reason
  - Parsed_Result
- 提取第一个 <...> 到 Bracket_Result
- Final_Decision 优先级：
  1) 先用 <x> 的 x
  2) 提取不到则尝试 JSON {"decision": ...}
- Reasoning_Content：尽力提取深度思考字段（若返回）

## 6. 输入 Excel 要求

- 文件格式：.xlsx
- 第一行是列名
- 每行一条样本
- 列数量不限

## 7. 输出 Excel 列说明

保留所有原始列，并新增：
- Raw_Output
- Reasoning_Content
- Parsed_Reason
- Parsed_Result
- Bracket_Result
- Final_Decision
- Prompt_Tokens
- Completion_Tokens
- Total_Tokens
- Estimated_Cost_USD
- Status

## 8. 安装方式

pip install pandas requests openpyxl

运行：
python runner.py

## 9. 快速使用步骤

1. 选择 Excel
2. 选择输出目录
3. 选择平台和模型，填写 API Key
4. 写前置/后置文本
5. 在提示词中要求模型输出格式：abc...<x>
6. 点击 Start

## 10. 注意事项

- 成本是估算值（基于本地价格表）
- 深度思考文本是否可获取取决于平台/模型返回
- Stop 为优雅停止：当前样本完成后停止
