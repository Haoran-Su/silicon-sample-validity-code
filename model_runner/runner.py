import tkinter as tk
from tkinter import ttk, filedialog, messagebox, scrolledtext
import pandas as pd
import requests
import json
import threading
import re
import time
import os


class ExperimentRunnerApp:
    def __init__(self, root):
        self.root = root
        self.root.title("LLM Experiment Runner")

        # Responsive default size
        sw = self.root.winfo_screenwidth()
        sh = self.root.winfo_screenheight()
        w = min(1200, max(1000, sw - 120))
        h = min(920, max(780, sh - 120))
        self.root.geometry(f"{w}x{h}")
        self.root.minsize(980, 720)
        self.root.configure(bg="#f5f6f7")

        self.file_path = None
        self.df = None
        self.is_running = False
        self.stop_requested = False

        self.config_file = "runner_config.json"
        self.output_dir = os.getcwd()

        self.models_by_provider = {
            "OpenRouter": [
                "deepseek/deepseek-r1",
                "deepseek/deepseek-v3",
                "openai/gpt-4o",
                "openai/gpt-5",
                "anthropic/claude-3.5-sonnet",
                "google/gemini-1.5-pro"
            ],
            "OpenAI": [
                "gpt-4o-mini",
                "gpt-4o",
                "gpt-5"
            ],
            "SiliconCloud": [
                "deepseek-ai/DeepSeek-R1",
                "deepseek-ai/DeepSeek-V3",
                "Qwen/Qwen2.5-72B-Instruct",
                "meta-llama/Meta-Llama-3.1-70B-Instruct"
            ]
        }

        self.api_keys = {"OpenRouter": "", "OpenAI": "", "SiliconCloud": ""}
        self.current_provider = "OpenRouter"

        # Example pricing (USD / 1M tokens). Update with your actual pricing.
        self.pricing_per_million = {
            "OpenRouter": {"default": {"input": 2.0, "output": 8.0}},
            "OpenAI": {
                "gpt-4o-mini": {"input": 0.15, "output": 0.60},
                "gpt-4o": {"input": 5.0, "output": 15.0},
                "gpt-5": {"input": 10.0, "output": 30.0},
                "default": {"input": 5.0, "output": 15.0}
            },
            "SiliconCloud": {"default": {"input": 1.0, "output": 3.0}}
        }

        self._setup_style()
        self._build_scrollable_page()
        self.load_config()
        self.setup_ui()
        self.apply_loaded_config_to_ui()

    # ---------------- Style ----------------
    def _setup_style(self):
        style = ttk.Style()
        style.theme_use("clam")

        bg = "#f5f6f7"
        panel = "#ffffff"
        border = "#d9dde3"
        text = "#1f2937"
        muted = "#4b5563"

        style.configure(".", background=bg, foreground=text, font=("Segoe UI", 10))
        style.configure("TFrame", background=bg)
        style.configure("Panel.TLabelframe", background=panel, bordercolor=border, relief="solid")
        style.configure("Panel.TLabelframe.Label", background=panel, foreground=text, font=("Segoe UI", 10, "bold"))

        style.configure("TLabel", background=bg, foreground=text)
        style.configure("Muted.TLabel", background=bg, foreground=muted)

        style.configure("TButton", padding=(10, 6))
        style.configure("TEntry", fieldbackground="#ffffff")
        style.configure("TCombobox", fieldbackground="#ffffff")
        style.configure("TCheckbutton", background=bg, foreground=text)

        style.configure("Horizontal.TProgressbar", troughcolor="#e5e7eb", background="#4b5563")

    def _build_scrollable_page(self):
        # Outer container
        self.outer = ttk.Frame(self.root)
        self.outer.pack(fill="both", expand=True)

        # Scrollable canvas
        self.canvas = tk.Canvas(
            self.outer,
            bg="#f5f6f7",
            highlightthickness=0,
            bd=0
        )
        self.vscroll = ttk.Scrollbar(self.outer, orient="vertical", command=self.canvas.yview)
        self.canvas.configure(yscrollcommand=self.vscroll.set)

        self.vscroll.pack(side="right", fill="y")
        self.canvas.pack(side="left", fill="both", expand=True)

        # Inner page frame
        self.page = ttk.Frame(self.canvas, padding=16)
        self.page_window = self.canvas.create_window((0, 0), window=self.page, anchor="nw")

        # Update scroll region
        self.page.bind("<Configure>", self._on_page_configure)
        self.canvas.bind("<Configure>", self._on_canvas_configure)

        # Mousewheel support
        self.canvas.bind_all("<MouseWheel>", self._on_mousewheel)      # Windows / macOS
        self.canvas.bind_all("<Button-4>", self._on_mousewheel_linux)  # Linux up
        self.canvas.bind_all("<Button-5>", self._on_mousewheel_linux)  # Linux down

    def _on_page_configure(self, _event=None):
        self.canvas.configure(scrollregion=self.canvas.bbox("all"))

    def _on_canvas_configure(self, event):
        # Keep inner frame width equal to canvas visible width
        self.canvas.itemconfig(self.page_window, width=event.width)

    def _on_mousewheel(self, event):
        # On Windows: event.delta is multiple of 120
        delta = -1 * int(event.delta / 120) if event.delta else 0
        self.canvas.yview_scroll(delta, "units")

    def _on_mousewheel_linux(self, event):
        if event.num == 4:
            self.canvas.yview_scroll(-1, "units")
        elif event.num == 5:
            self.canvas.yview_scroll(1, "units")

    # ---------------- UI ----------------
    def setup_ui(self):
        # Configuration
        config_frame = ttk.LabelFrame(self.page, text="Configuration", style="Panel.TLabelframe", padding=14)
        config_frame.pack(fill="x", pady=(0, 10))

        # Use a grid with stable column sizing
        config_frame.columnconfigure(0, weight=0)
        config_frame.columnconfigure(1, weight=1)
        config_frame.columnconfigure(2, weight=0)
        config_frame.columnconfigure(3, weight=1)

        ttk.Button(config_frame, text="Select Excel File", command=self.load_file)\
            .grid(row=0, column=0, sticky="w", pady=4)
        self.lbl_file = ttk.Label(config_frame, text="No file selected", style="Muted.TLabel")
        self.lbl_file.grid(row=0, column=1, sticky="we", padx=10, pady=4)

        ttk.Button(config_frame, text="Select Output Folder", command=self.choose_output_dir)\
            .grid(row=0, column=2, sticky="w", padx=(18, 0), pady=4)
        self.lbl_output_dir = ttk.Label(config_frame, text=self.output_dir, style="Muted.TLabel")
        self.lbl_output_dir.grid(row=0, column=3, sticky="we", padx=10, pady=4)

        ttk.Label(config_frame, text="Provider").grid(row=1, column=0, sticky="w", pady=4)
        self.combo_provider = ttk.Combobox(
            config_frame, width=24, state="readonly", values=list(self.models_by_provider.keys())
        )
        self.combo_provider.grid(row=1, column=1, sticky="w", padx=10, pady=4)
        self.combo_provider.bind("<<ComboboxSelected>>", self.on_provider_change)

        ttk.Label(config_frame, text="API Key").grid(row=2, column=0, sticky="w", pady=4)
        self.entry_api = ttk.Entry(config_frame, width=44, show="*")
        self.entry_api.grid(row=2, column=1, sticky="w", padx=10, pady=4)
        self.entry_api.bind("<FocusOut>", lambda e: self.persist_current_provider_key())

        ttk.Label(config_frame, text="Model").grid(row=3, column=0, sticky="w", pady=4)
        self.combo_model = ttk.Combobox(config_frame, width=41, state="readonly")
        self.combo_model.grid(row=3, column=1, sticky="w", padx=10, pady=4)

        self.rerun_failed_var = tk.BooleanVar(value=False)
        ttk.Checkbutton(
            config_frame,
            text="Retry failed rows only (Final_Decision is empty)",
            variable=self.rerun_failed_var
        ).grid(row=4, column=0, columnspan=2, sticky="w", pady=4)

        param_frame = ttk.LabelFrame(config_frame, text="Generation Parameters", style="Panel.TLabelframe", padding=10)
        param_frame.grid(row=1, column=2, rowspan=6, padx=(18, 0), sticky="n")

        label_w = 13
        entry_w = 12

        ttk.Label(param_frame, text="temperature", width=label_w).grid(row=0, column=0, sticky="w", pady=2)
        self.entry_temperature = ttk.Entry(param_frame, width=entry_w)
        self.entry_temperature.grid(row=0, column=1, padx=8, pady=2)

        ttk.Label(param_frame, text="top_p", width=label_w).grid(row=1, column=0, sticky="w", pady=2)
        self.entry_top_p = ttk.Entry(param_frame, width=entry_w)
        self.entry_top_p.grid(row=1, column=1, padx=8, pady=2)

        ttk.Label(param_frame, text="max_tokens", width=label_w).grid(row=2, column=0, sticky="w", pady=2)
        self.entry_max_tokens = ttk.Entry(param_frame, width=entry_w)
        self.entry_max_tokens.grid(row=2, column=1, padx=8, pady=2)

        ttk.Label(param_frame, text="timeout_sec", width=label_w).grid(row=3, column=0, sticky="w", pady=2)
        self.entry_timeout = ttk.Entry(param_frame, width=entry_w)
        self.entry_timeout.grid(row=3, column=1, padx=8, pady=2)

        ttk.Label(param_frame, text="retries", width=label_w).grid(row=4, column=0, sticky="w", pady=2)
        self.entry_retries = ttk.Entry(param_frame, width=entry_w)
        self.entry_retries.grid(row=4, column=1, padx=8, pady=2)

        ttk.Label(param_frame, text="interval_sec", width=label_w).grid(row=5, column=0, sticky="w", pady=2)
        self.entry_interval = ttk.Entry(param_frame, width=entry_w)
        self.entry_interval.grid(row=5, column=1, padx=8, pady=2)

        ttk.Label(param_frame, text="separator", width=label_w).grid(row=6, column=0, sticky="w", pady=2)
        self.entry_separator = ttk.Entry(param_frame, width=18)
        self.entry_separator.grid(row=6, column=1, padx=8, pady=2)
        self.entry_separator.insert(0, "<<<SEP>>>")

        # Prompt Builder
        prompt_frame = ttk.LabelFrame(self.page, text="Prompt Builder", style="Panel.TLabelframe", padding=14)
        prompt_frame.pack(fill="x", pady=(0, 10))

        ttk.Label(
            prompt_frame,
            text=("Prompt format: prefix + middle profile string + suffix. "
                  "Middle format is fixed: your {column} is {value}； your ..."),
            style="Muted.TLabel"
        ).pack(anchor="w")

        tb = ttk.Frame(prompt_frame)
        tb.pack(fill="x", expand=True, pady=(8, 0))
        tb.columnconfigure(0, weight=1)
        tb.columnconfigure(1, weight=1)

        left_frame = ttk.Frame(tb)
        left_frame.grid(row=0, column=0, sticky="nsew", padx=(0, 8))
        ttk.Label(left_frame, text="Prefix text").pack(anchor="w")
        self.text_prompt_prefix = scrolledtext.ScrolledText(
            left_frame, height=5, wrap=tk.WORD, bg="#ffffff", fg="#111827",
            insertbackground="#111827", bd=1, relief="solid"
        )
        self.text_prompt_prefix.pack(fill="both", expand=True)

        right_frame = ttk.Frame(tb)
        right_frame.grid(row=0, column=1, sticky="nsew", padx=(8, 0))
        ttk.Label(right_frame, text="Suffix text").pack(anchor="w")
        self.text_prompt_suffix = scrolledtext.ScrolledText(
            right_frame, height=5, wrap=tk.WORD, bg="#ffffff", fg="#111827",
            insertbackground="#111827", bd=1, relief="solid"
        )
        self.text_prompt_suffix.pack(fill="both", expand=True)

        # Current Sample Preview
        persona_frame = ttk.LabelFrame(self.page, text="Current Sample Preview", style="Panel.TLabelframe", padding=14)
        persona_frame.pack(fill="both", expand=True, pady=(0, 10))

        ttk.Label(
            persona_frame,
            text="Shows the current sample profile string and exact prompt sent to the API.",
            style="Muted.TLabel"
        ).pack(anchor="w")

        self.text_current_persona = scrolledtext.ScrolledText(
            persona_frame, height=10, wrap=tk.WORD, bg="#ffffff", fg="#111827",
            insertbackground="#111827", bd=1, relief="solid"
        )
        self.text_current_persona.pack(fill="both", expand=True, pady=(6, 0))
        self.text_current_persona.insert(tk.END, "Not running yet.")
        self.text_current_persona.config(state="disabled")

        # Run Controls
        run_frame = ttk.Frame(self.page)
        run_frame.pack(fill="x", pady=(0, 8))

        self.btn_run = ttk.Button(run_frame, text="Start", command=self.start_batch_thread, width=12)
        self.btn_run.pack(side="left")

        self.btn_stop = ttk.Button(run_frame, text="Stop", command=self.stop_batch, state="disabled", width=12)
        self.btn_stop.pack(side="left", padx=(8, 0))

        self.progress_var = tk.DoubleVar()
        self.progress_bar = ttk.Progressbar(
            run_frame, variable=self.progress_var, maximum=100, style="Horizontal.TProgressbar"
        )
        self.progress_bar.pack(side="left", fill="x", expand=True, padx=10)

        self.lbl_progress = ttk.Label(run_frame, text="0 / 0", width=10, anchor="e")
        self.lbl_progress.pack(side="right")

        # Log
        log_frame = ttk.LabelFrame(self.page, text="Run Log", style="Panel.TLabelframe", padding=10)
        log_frame.pack(fill="both", expand=True)

        self.log_console = scrolledtext.ScrolledText(
            log_frame, height=12, wrap=tk.WORD, state='disabled',
            bg="#f8fafc", fg="#111827", insertbackground="#111827", bd=1, relief="solid"
        )
        self.log_console.pack(fill="both", expand=True)

        self.root.protocol("WM_DELETE_WINDOW", self.on_close)

    # ---------------- Config ----------------
    def load_config(self):
        if not os.path.exists(self.config_file):
            return
        try:
            with open(self.config_file, "r", encoding="utf-8") as f:
                cfg = json.load(f)

            self.api_keys.update(cfg.get("api_keys", {}))
            self.current_provider = cfg.get("provider", "OpenRouter")
            self.output_dir = cfg.get("output_dir", os.getcwd())

            self.loaded_model = cfg.get("model", "")
            self.loaded_temperature = cfg.get("temperature", "0.6")
            self.loaded_top_p = cfg.get("top_p", "1.0")
            self.loaded_max_tokens = cfg.get("max_tokens", "512")
            self.loaded_timeout = cfg.get("timeout", "120")
            self.loaded_retries = cfg.get("retries", "3")
            self.loaded_interval = cfg.get("interval", "0.5")
            self.loaded_separator = cfg.get("separator", "<<<SEP>>>")
            self.loaded_rerun_failed = cfg.get("rerun_failed_only", False)
            self.loaded_prompt_prefix = cfg.get("prompt_prefix", "")
            self.loaded_prompt_suffix = cfg.get("prompt_suffix", "")
        except Exception:
            pass

    def save_config(self):
        try:
            self.persist_current_provider_key()
            cfg = {
                "api_keys": self.api_keys,
                "provider": self.combo_provider.get().strip() if hasattr(self, "combo_provider") else self.current_provider,
                "model": self.combo_model.get().strip() if hasattr(self, "combo_model") else "",
                "output_dir": self.output_dir,
                "temperature": self.entry_temperature.get().strip() if hasattr(self, "entry_temperature") else "0.6",
                "top_p": self.entry_top_p.get().strip() if hasattr(self, "entry_top_p") else "1.0",
                "max_tokens": self.entry_max_tokens.get().strip() if hasattr(self, "entry_max_tokens") else "512",
                "timeout": self.entry_timeout.get().strip() if hasattr(self, "entry_timeout") else "120",
                "retries": self.entry_retries.get().strip() if hasattr(self, "entry_retries") else "3",
                "interval": self.entry_interval.get().strip() if hasattr(self, "entry_interval") else "0.5",
                "separator": self.entry_separator.get().strip() if hasattr(self, "entry_separator") else "<<<SEP>>>",
                "rerun_failed_only": self.rerun_failed_var.get() if hasattr(self, "rerun_failed_var") else False,
                "prompt_prefix": self.text_prompt_prefix.get("1.0", tk.END).strip() if hasattr(self, "text_prompt_prefix") else "",
                "prompt_suffix": self.text_prompt_suffix.get("1.0", tk.END).strip() if hasattr(self, "text_prompt_suffix") else ""
            }
            with open(self.config_file, "w", encoding="utf-8") as f:
                json.dump(cfg, f, ensure_ascii=False, indent=2)
        except Exception as e:
            self.log(f"Failed to save config: {e}")

    def apply_loaded_config_to_ui(self):
        providers = list(self.models_by_provider.keys())
        if self.current_provider not in providers:
            self.current_provider = providers[0]

        self.combo_provider.set(self.current_provider)
        self.refresh_model_list()

        if hasattr(self, "loaded_model") and self.loaded_model in self.combo_model["values"]:
            self.combo_model.set(self.loaded_model)
        elif len(self.combo_model["values"]) > 0:
            self.combo_model.current(0)

        self.entry_api.delete(0, tk.END)
        self.entry_api.insert(0, self.api_keys.get(self.current_provider, ""))

        self.entry_temperature.insert(0, getattr(self, "loaded_temperature", "0.6"))
        self.entry_top_p.insert(0, getattr(self, "loaded_top_p", "1.0"))
        self.entry_max_tokens.insert(0, getattr(self, "loaded_max_tokens", "512"))
        self.entry_timeout.insert(0, getattr(self, "loaded_timeout", "120"))
        self.entry_retries.insert(0, getattr(self, "loaded_retries", "3"))
        self.entry_interval.insert(0, getattr(self, "loaded_interval", "0.5"))
        self.entry_separator.delete(0, tk.END)
        self.entry_separator.insert(0, getattr(self, "loaded_separator", "<<<SEP>>>"))

        self.rerun_failed_var.set(getattr(self, "loaded_rerun_failed", False))
        self.text_prompt_prefix.delete("1.0", tk.END)
        self.text_prompt_prefix.insert(tk.END, getattr(self, "loaded_prompt_prefix", ""))
        self.text_prompt_suffix.delete("1.0", tk.END)
        self.text_prompt_suffix.insert(tk.END, getattr(self, "loaded_prompt_suffix", ""))

        self.lbl_output_dir.config(text=self.output_dir)

    # ---------------- Helpers ----------------
    def log(self, message):
        self.root.after(0, self._log_to_console, message)

    def _log_to_console(self, message):
        self.log_console.config(state='normal')
        self.log_console.insert(tk.END, message + "\n")
        self.log_console.see(tk.END)
        self.log_console.config(state='disabled')

    def log_separator(self):
        self.log("--------------------------------------------------")

    def update_progress(self, current, total):
        self.root.after(0, self._update_progress_ui, current, total)

    def _update_progress_ui(self, current, total):
        if total > 0:
            self.progress_var.set((current / total) * 100)
        else:
            self.progress_var.set(0)
        self.lbl_progress.config(text=f"{current} / {total}")

    def update_current_persona_preview(self, text):
        self.root.after(0, self._set_current_persona_preview, text)

    def _set_current_persona_preview(self, text):
        self.text_current_persona.config(state="normal")
        self.text_current_persona.delete("1.0", tk.END)
        self.text_current_persona.insert(tk.END, text)
        self.text_current_persona.see(tk.END)
        self.text_current_persona.config(state="disabled")

    def persist_current_provider_key(self):
        if hasattr(self, "combo_provider") and hasattr(self, "entry_api"):
            provider = self.combo_provider.get().strip()
            self.api_keys[provider] = self.entry_api.get().strip()

    def on_provider_change(self, _event=None):
        old_provider = self.current_provider
        self.api_keys[old_provider] = self.entry_api.get().strip()

        self.current_provider = self.combo_provider.get().strip()
        self.refresh_model_list()

        self.entry_api.delete(0, tk.END)
        self.entry_api.insert(0, self.api_keys.get(self.current_provider, ""))

        self.save_config()

    def refresh_model_list(self):
        provider = self.combo_provider.get().strip()
        model_list = self.models_by_provider.get(provider, [])
        self.combo_model["values"] = model_list
        if model_list:
            self.combo_model.current(0)
        else:
            self.combo_model.set("")

    def load_file(self):
        filepath = filedialog.askopenfilename(filetypes=[("Excel files", "*.xlsx")])
        if filepath:
            self.file_path = filepath
            self.lbl_file.config(text=os.path.basename(filepath), foreground="#1f2937")
            try:
                self.df = pd.read_excel(filepath)
                self.log_separator()
                self.log(f"File loaded: {os.path.basename(filepath)}")
                self.log(f"Row count: {len(self.df)}")
                self.log(f"Columns: {', '.join(self.df.columns.tolist())}")
            except Exception as e:
                messagebox.showerror("Read Error", f"Unable to read Excel file:\n{str(e)}")

    def choose_output_dir(self):
        selected = filedialog.askdirectory(initialdir=self.output_dir)
        if selected:
            self.output_dir = selected
            self.lbl_output_dir.config(text=self.output_dir, foreground="#1f2937")
            self.save_config()

    def stop_batch(self):
        if self.is_running:
            self.stop_requested = True
            self.log("Stop requested. The current row will finish before stopping.")

    def on_close(self):
        self.save_config()
        self.root.destroy()

    # ---------------- Prompt format ----------------
    def format_cell_value(self, value):
        if pd.isna(value):
            return ""
        return str(value).strip()

    def build_middle_persona_string(self, row, exclude_cols):
        parts = []
        for col in self.df.columns:
            if col in exclude_cols:
                continue
            v = self.format_cell_value(row[col])
            parts.append(f"your {col} is {v}")
        return "； ".join(parts)

    def build_full_prompt(self, row, exclude_cols, prefix_text, suffix_text):
        middle = self.build_middle_persona_string(row, exclude_cols)
        blocks = []
        if prefix_text.strip():
            blocks.append(prefix_text.strip())
        blocks.append(middle)
        if suffix_text.strip():
            blocks.append(suffix_text.strip())
        return "\n\n".join(blocks), middle

    # ---------------- Result parsing ----------------
    def extract_bracket_result(self, text):
        """
        Extract the first content wrapped by angle brackets.
        Example: <x> -> x
        """
        if not isinstance(text, str):
            return None
        m = re.search(r'<\s*([^<>]+?)\s*>', text)
        if m:
            return m.group(1).strip()
        return None

    def parse_decision_from_text(self, text):
        """
        Parse decision from JSON-like snippet in text:
        {"decision": 1}
        """
        if not isinstance(text, str):
            return None
        m = re.search(r'\{[^{}]*"decision"\s*:\s*-?\d+(\.\d+)?[^}]*\}', text, re.IGNORECASE)
        if m:
            try:
                obj = json.loads(m.group(0))
                return obj.get("decision", None)
            except Exception:
                return None
        return None

    def cast_bracket_to_number_if_possible(self, bracket_value):
        """
        Try to cast bracket value to int/float.
        Return original string if casting fails.
        """
        if bracket_value is None:
            return None
        v = bracket_value.strip()
        try:
            if re.fullmatch(r"-?\d+", v):
                return int(v)
            if re.fullmatch(r"-?\d+\.\d+", v):
                return float(v)
        except Exception:
            pass
        return v

    def extract_reasoning_text(self, result_json):
        """
        Try best-effort extraction of reasoning/deep-thinking text from multiple schemas.
        Returns "" if unavailable.
        """
        reasoning_candidates = []

        # 1) Top-level fields
        for k in ["reasoning", "reasoning_content", "thinking", "thoughts"]:
            v = result_json.get(k)
            if isinstance(v, str) and v.strip():
                reasoning_candidates.append(v.strip())

        # 2) choices[0] level
        choices = result_json.get("choices", [])
        if choices and isinstance(choices[0], dict):
            c0 = choices[0]

            for k in ["reasoning", "reasoning_content", "thinking", "thoughts"]:
                v = c0.get(k)
                if isinstance(v, str) and v.strip():
                    reasoning_candidates.append(v.strip())

            # 3) message level
            msg = c0.get("message", {})
            if isinstance(msg, dict):
                for k in ["reasoning", "reasoning_content", "thinking", "thoughts"]:
                    v = msg.get(k)
                    if isinstance(v, str) and v.strip():
                        reasoning_candidates.append(v.strip())

                # Some providers may return structured content arrays
                content = msg.get("content")
                if isinstance(content, list):
                    for item in content:
                        if isinstance(item, dict):
                            # e.g. {"type":"reasoning","text":"..."}
                            t = str(item.get("type", "")).lower()
                            txt = item.get("text")
                            if t in ["reasoning", "thinking", "thought"] and isinstance(txt, str) and txt.strip():
                                reasoning_candidates.append(txt.strip())

            # 4) delta level (stream-like or transformed response)
            delta = c0.get("delta", {})
            if isinstance(delta, dict):
                for k in ["reasoning", "reasoning_content", "thinking", "thoughts"]:
                    v = delta.get(k)
                    if isinstance(v, str) and v.strip():
                        reasoning_candidates.append(v.strip())

        if reasoning_candidates:
            # Deduplicate while preserving order
            seen = set()
            uniq = []
            for x in reasoning_candidates:
                key = x.strip()
                if key and key not in seen:
                    seen.add(key)
                    uniq.append(key)
            return "\n\n".join(uniq).strip()

        return ""

    # ---------------- API ----------------
    def get_endpoint_and_headers(self, provider, api_key):
        if provider == "OpenRouter":
            return "https://openrouter.ai/api/v1/chat/completions", {
                "Authorization": f"Bearer {api_key}",
                "Content-Type": "application/json"
            }
        elif provider == "OpenAI":
            return "https://api.openai.com/v1/chat/completions", {
                "Authorization": f"Bearer {api_key}",
                "Content-Type": "application/json"
            }
        elif provider == "SiliconCloud":
            return "https://api.siliconflow.cn/v1/chat/completions", {
                "Authorization": f"Bearer {api_key}",
                "Content-Type": "application/json"
            }
        else:
            raise ValueError(f"Unknown provider: {provider}")

    def split_reason_and_result(self, raw_output, separator):
        if not isinstance(raw_output, str):
            return "", ""
        if separator and separator in raw_output:
            reason, result = raw_output.split(separator, 1)
            return reason.strip(), result.strip()
        return raw_output.strip(), ""

    def extract_text_usage_reasoning(self, result_json):
        raw_content = result_json["choices"][0]["message"]["content"]
        reasoning_text = self.extract_reasoning_text(result_json)

        usage = result_json.get("usage", {})
        prompt_tokens = int(usage.get("prompt_tokens", 0) or 0)
        completion_tokens = int(usage.get("completion_tokens", 0) or 0)
        total_tokens = int(usage.get("total_tokens", prompt_tokens + completion_tokens) or 0)

        return raw_content, reasoning_text, prompt_tokens, completion_tokens, total_tokens

    def estimate_cost_usd(self, provider, model, prompt_tokens, completion_tokens):
        provider_table = self.pricing_per_million.get(provider, {})
        price = provider_table.get(model, provider_table.get("default", {"input": 0.0, "output": 0.0}))
        in_cost = prompt_tokens / 1_000_000 * price["input"]
        out_cost = completion_tokens / 1_000_000 * price["output"]
        return in_cost + out_cost

    def call_llm(self, provider, api_key, model, prompt, temperature, top_p, max_tokens, timeout_sec, max_retries):
        endpoint, headers = self.get_endpoint_and_headers(provider, api_key)
        payload = {
            "model": model,
            "messages": [{"role": "user", "content": prompt}],
            "temperature": temperature,
            "top_p": top_p,
            "max_tokens": max_tokens
        }

        for attempt in range(max_retries):
            try:
                resp = requests.post(endpoint, headers=headers, json=payload, timeout=timeout_sec)
                resp.raise_for_status()
                result_json = resp.json()
                return self.extract_text_usage_reasoning(result_json)
            except Exception as e:
                self.log(f"Request failed ({attempt + 1}/{max_retries}): {e}")
                time.sleep(2)

        return "ERROR", "", 0, 0, 0

    # ---------------- Batch ----------------
    def start_batch_thread(self):
        if not self.file_path or self.df is None:
            messagebox.showwarning("Missing File", "Please select an Excel file first.")
            return

        provider = self.combo_provider.get().strip()
        api_key = self.entry_api.get().strip()
        model = self.combo_model.get().strip()

        if not api_key:
            messagebox.showwarning("Missing API Key", "Please enter an API key.")
            return
        if not model:
            messagebox.showwarning("Missing Model", "Please select a model.")
            return

        self.api_keys[provider] = api_key
        self.save_config()

        self.stop_requested = False
        self.is_running = True
        self.btn_run.config(state="disabled")
        self.btn_stop.config(state="normal")

        threading.Thread(target=self.run_batch_logic, daemon=True).start()

    def run_batch_logic(self):
        try:
            provider = self.combo_provider.get().strip()
            api_key = self.entry_api.get().strip()
            model = self.combo_model.get().strip()

            temperature = float(self.entry_temperature.get().strip())
            top_p = float(self.entry_top_p.get().strip())
            max_tokens = int(self.entry_max_tokens.get().strip())
            timeout_sec = int(self.entry_timeout.get().strip())
            max_retries = int(self.entry_retries.get().strip())
            interval_sec = float(self.entry_interval.get().strip())
            separator = self.entry_separator.get().strip()
            rerun_failed_only = self.rerun_failed_var.get()

            prompt_prefix = self.text_prompt_prefix.get("1.0", tk.END).strip()
            prompt_suffix = self.text_prompt_suffix.get("1.0", tk.END).strip()

            if not separator:
                raise ValueError("Separator cannot be empty.")

            required_cols = {
                "Raw_Output": None,
                "Reasoning_Content": None,   # New column for deep-thinking text
                "Parsed_Reason": None,
                "Parsed_Result": None,
                "Bracket_Result": None,
                "Final_Decision": None,
                "Prompt_Tokens": 0,
                "Completion_Tokens": 0,
                "Total_Tokens": 0,
                "Estimated_Cost_USD": 0.0,
                "Status": ""
            }
            for c, default_v in required_cols.items():
                if c not in self.df.columns:
                    self.df[c] = default_v

            if rerun_failed_only:
                target_indices = self.df[self.df["Final_Decision"].isna()].index.tolist()
            else:
                target_indices = self.df.index.tolist()

            total_rows = len(target_indices)
            done_count = 0

            sum_prompt_tokens = 0
            sum_completion_tokens = 0
            sum_total_tokens = 0
            sum_cost_usd = 0.0
            success_count = 0
            failed_count = 0
            reasoning_hit_count = 0

            self.log_separator()
            self.log(f"Run started: provider={provider}, model={model}")
            self.log(f"Mode: {'retry_failed_only' if rerun_failed_only else 'full'}")
            self.log(f"Target rows: {total_rows}")
            self.log(f"Separator: {separator}")
            self.log("Decision priority: <x> first, then JSON {'decision': ...}")
            self.log("Reasoning capture: enabled")
            self.log_separator()

            if total_rows == 0:
                self.log("No rows to process.")
                self.update_progress(0, 0)
                return

            exclude_cols = set(required_cols.keys())

            for idx in target_indices:
                if self.stop_requested:
                    self.log("Stop signal received. Ending batch.")
                    break

                row = self.df.loc[idx]
                full_prompt, middle_persona = self.build_full_prompt(
                    row=row,
                    exclude_cols=exclude_cols,
                    prefix_text=prompt_prefix,
                    suffix_text=prompt_suffix
                )

                preview_text = (
                    f"[Row Index] {idx}\n"
                    f"[Provider/Model] {provider} / {model}\n\n"
                    f"[Middle Profile String]\n{middle_persona}\n\n"
                    f"[Final Prompt Sent]\n{full_prompt}\n"
                )
                self.update_current_persona_preview(preview_text)

                self.log(f"Processing row index={idx} ({done_count + 1}/{total_rows})")

                raw_output, reasoning_text, ptk, ctk, ttk_tokens = self.call_llm(
                    provider, api_key, model, full_prompt,
                    temperature, top_p, max_tokens, timeout_sec, max_retries
                )

                reason_text, result_text = self.split_reason_and_result(raw_output, separator)

                # 1) Bracket extraction (preferred)
                bracket_result = self.extract_bracket_result(result_text if result_text else raw_output)

                # 2) Final decision selection logic
                decision = self.cast_bracket_to_number_if_possible(bracket_result)

                # 3) Fallback to JSON decision if bracket not found
                if decision is None:
                    decision = self.parse_decision_from_text(result_text if result_text else raw_output)

                est_cost = self.estimate_cost_usd(provider, model, ptk, ctk)

                self.df.at[idx, "Raw_Output"] = raw_output
                self.df.at[idx, "Reasoning_Content"] = reasoning_text if reasoning_text else None
                self.df.at[idx, "Parsed_Reason"] = reason_text
                self.df.at[idx, "Parsed_Result"] = result_text
                self.df.at[idx, "Bracket_Result"] = bracket_result
                self.df.at[idx, "Final_Decision"] = decision
                self.df.at[idx, "Prompt_Tokens"] = int(ptk)
                self.df.at[idx, "Completion_Tokens"] = int(ctk)
                self.df.at[idx, "Total_Tokens"] = int(ttk_tokens)
                self.df.at[idx, "Estimated_Cost_USD"] = float(est_cost)

                if reasoning_text and str(reasoning_text).strip():
                    reasoning_hit_count += 1

                if raw_output != "ERROR":
                    self.df.at[idx, "Status"] = "SUCCESS"
                    success_count += 1
                    self.log(
                        f"Row status=SUCCESS, bracket_result={bracket_result}, "
                        f"final_decision={decision}, reasoning_captured={'yes' if reasoning_text else 'no'}, "
                        f"tokens={ttk_tokens}, cost_usd={est_cost:.6f}"
                    )
                else:
                    self.df.at[idx, "Status"] = "FAILED"
                    failed_count += 1
                    self.log("Row status=FAILED")

                sum_prompt_tokens += int(ptk)
                sum_completion_tokens += int(ctk)
                sum_total_tokens += int(ttk_tokens)
                sum_cost_usd += float(est_cost)

                done_count += 1
                self.update_progress(done_count, total_rows)
                time.sleep(max(0.0, interval_sec))

            timestamp = int(time.time())
            mode_tag = "rerun_failed" if rerun_failed_only else "full"
            output_filename = f"Experiment_Result_{mode_tag}_{timestamp}.xlsx"
            output_path = os.path.join(self.output_dir, output_filename)
            self.df.to_excel(output_path, index=False)

            self.log_separator()
            self.log("RUN SUMMARY")
            self.log(f"Provider/Model: {provider} / {model}")
            self.log(f"Processed: {done_count}/{total_rows}")
            self.log(f"SUCCESS: {success_count}")
            self.log(f"FAILED: {failed_count}")
            self.log(f"Reasoning captured rows: {reasoning_hit_count}")
            self.log(f"Prompt tokens: {sum_prompt_tokens}")
            self.log(f"Completion tokens: {sum_completion_tokens}")
            self.log(f"Total tokens: {sum_total_tokens}")
            self.log(f"Estimated total cost (USD): {sum_cost_usd:.6f}")
            self.log(f"Output file: {output_path}")
            self.log_separator()

            self.root.after(
                0,
                lambda: messagebox.showinfo(
                    "Run Completed",
                    f"Processed: {done_count}/{total_rows}\n"
                    f"SUCCESS: {success_count}\n"
                    f"FAILED: {failed_count}\n"
                    f"Reasoning captured rows: {reasoning_hit_count}\n"
                    f"Estimated cost (USD): {sum_cost_usd:.6f}\n\n"
                    f"Saved to:\n{output_path}"
                )
            )

        except ValueError as ve:
            self.log(f"Parameter error: {ve}")
            self.root.after(0, lambda: messagebox.showerror("Parameter Error", f"{ve}"))
        except Exception as e:
            self.log(f"Runtime error: {e}")
            self.root.after(0, lambda: messagebox.showerror("Runtime Error", str(e)))
        finally:
            self.is_running = False
            self.stop_requested = False
            self.save_config()
            self.root.after(0, lambda: self.btn_run.config(state="normal"))
            self.root.after(0, lambda: self.btn_stop.config(state="disabled"))


if __name__ == "__main__":
    root = tk.Tk()
    app = ExperimentRunnerApp(root)
    root.mainloop()