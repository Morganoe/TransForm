#! /usr/bin/env python3

from tkinter import Tk, filedialog, IntVar, Text
from tkinter import ttk
import tkinter as tk

from pathlib import Path

from pprint import pprint

#########################
### CLASS DEFINITIONS ###
#########################

class AnalysisWindow:
    def __init__(self, master):
        self.combo_index = 0
        self.fname = "analysis.config"

        self.master = master
        self.master.title("TransForm")

        self.nb = ttk.Notebook(self.master)

        self.page_main = ttk.Frame(self.nb)


        self.input_dir_btn = ttk.Button(master = self.page_main, \
                                        text = "Input Directory",
                                        command = lambda:self._get_directory(self.input_dir_field))

        self.input_dir_field = ttk.Entry(master = self.page_main)

        self.output_dir_btn = ttk.Button(master = self.page_main, \
                                         text = "Output Directory", \
                                         command = lambda:self._get_directory(self.output_dir_field))

        self.output_dir_field = ttk.Entry(master = self.page_main)

        self.output_file_field = ttk.Entry(master = self.page_main)
        self.output_file_label = ttk.Label(master = self.page_main, \
                                           text = "Output Filename")

        self.mode_btn_group = ttk.LabelFrame(master = self.page_main, \
                                             text = "Analysis Modes", \
                                             labelanchor = "nw", \
                                             borderwidth = 2,
                                             relief = "groove")

        self.mm2_val = IntVar()
        self.percentages_val = IntVar()
        self.totals_val = IntVar()

        self.mm2_val.set(0)
        self.percentages_val.set(0)
        self.totals_val.set(0)

        self.mode_mm2_btn = ttk.Checkbutton(master = self.mode_btn_group,
                                            text = "MM2",
                                            variable = self.mm2_val)

        self.mode_percentages_btn = ttk.Checkbutton(master = self.mode_btn_group,
                                            text = "Percentages",
                                            variable = self.percentages_val)

        self.mode_totals_btn = ttk.Checkbutton(master = self.mode_btn_group,
                                            text = "Totals",
                                            variable = self.totals_val)

        self.console_output = tk.Text(master = self.page_main, \
                                      width = 40, \
                                      height = 10, \
                                      state = "disabled", \
                                      wrap = "none")

        self.create_analysis_file = ttk.Button(master = self.page_main, \
                                               text = "Generate Analysis File",
                                               command = self._create_analysis_file)

        self.load_from_file_btn = ttk.Button(master = self.page_main,
                                             text = "Load From File",
                                             command = self._load_from_file)

        self.analyze_btn = ttk.Button(master = self.page_main,
                                      text = "Run Analysis",
                                      command = self._run_analysis)


        self.input_dir_field.grid(row = 0, column = 0)
        self.input_dir_btn.grid(row = 0, column = 1)

        self.output_dir_field.grid(row = 1, column = 0)
        self.output_dir_btn.grid(row = 1, column = 1)

        self.output_file_field.grid(row = 2, column = 0)
        self.output_file_label.grid(row = 2, column = 1)

        self.mode_btn_group.grid(row = 4, columnspan = 2)
        self.mode_mm2_btn.grid(row = 0, column = 0)
        self.mode_percentages_btn.grid(row = 0, column = 1)
        self.mode_totals_btn.grid(row = 0, column = 2)

        self.console_output.grid(row = 5, columnspan = 2)

        self.create_analysis_file.grid(row = 6, column = 0)
        self.load_from_file_btn.grid(row = 6, column = 1)
        self.analyze_btn.grid(row = 7, columnspan = 2)


        self.page_main.columnconfigure(0, weight = 1)
        self.page_main.columnconfigure(1, weight = 1)
        self.page_main.rowconfigure(0, weight = 1)
        self.page_main.rowconfigure(1, weight = 1)
        self.page_main.rowconfigure(2, weight = 1)
        self.page_main.rowconfigure(3, weight = 1)
        self.page_main.rowconfigure(4, weight = 1)
        self.page_main.rowconfigure(5, weight = 1)
        self.page_main.rowconfigure(6, weight = 1)


        self.page_main.pack(fill = "both", expand = True)


        self.page_combos = ttk.Frame(self.nb)

        self.combos_pane = VerticalScrolledFrame(master = self.page_combos)

        self.combos_label_phenotypes = ttk.Label(master = self.page_combos, \
                                                 text = "Phenotypes")

        self.combos_label_markers = ttk.Label(master = self.page_combos, \
                                              text = "Markers")

        ttk.Entry(master = self.combos_pane.interior).grid(
                                                        row = int(self.combo_index/2), 
                                                        column = self.combo_index%2)
        self.combo_index += 1
        ttk.Entry(master = self.combos_pane.interior).grid(
                                                        row = int(self.combo_index/2), 
                                                        column = self.combo_index%2)
        self.combo_index += 1


        self.add_combo_btn = ttk.Button(master = self.page_combos, \
                                        text = "Add combination",
                                        command = self._add_combo_to_frame)

        self.combos_pane.grid(row = 1, columnspan = 2)
        self.combos_label_phenotypes.grid(row = 0, column = 0)
        self.combos_label_markers.grid(row = 0, column = 1)

        self.add_combo_btn.grid(row = 2, columnspan = 2)

        self.page_combos.pack(fill = "both", expand = True)

        self.nb.add(self.page_main, text = "Analysis Options")
        self.nb.add(self.page_combos, text = "Combinations")

        self.nb.pack(expand = 1, fill = "both")


    def _get_directory(self, field):
        file = filedialog.askdirectory()
        field.delete(0, tk.END)
        field.insert(0, file)


    def _add_combo_to_frame(self):
        ttk.Entry(master = self.combos_pane.interior).grid(
                                                        row = int(self.combo_index/2), 
                                                        column = self.combo_index%2)
        self.combo_index += 1
        ttk.Entry(master = self.combos_pane.interior).grid(
                                                        row = int(self.combo_index/2), 
                                                        column = self.combo_index%2)
        self.combo_index += 1


    def _create_analysis_file(self):
        str_buf = ""

        str_buf += "INPUT DIR:  " + self.input_dir_field.get() + "\n"
        str_buf += "OUTPUT DIR:  " + self.output_dir_field.get() + "\n"
        str_buf += "OUTPUT FILENAME:  " + self.output_file_field.get() + "\n"
        str_buf += "MODES:  " + \
                str(self.mm2_val.get()) + \
                str(self.percentages_val.get()) + \
                str(self.totals_val.get()) + \
                "\n"

        combo_fields = self.combos_pane.interior.winfo_children()
        for entry_index in range(len(combo_fields)):
            if (entry_index % 2) == 0:
                str_buf += "PHENOTYPES:  " + combo_fields[entry_index].get() + "\n"
            else:
                str_buf += "MARKERS:  " + combo_fields[entry_index].get() + "\n"

        # Check if file already exists
        path = Path("./" + self.fname)
        # (Over)write to file
        with path.open(mode = "w") as f:
            f.write(str_buf)


    def _load_from_file(self):
        def _set_modes(payload):
            self.mm2_val.set(payload[0])
            self.percentages_val.set(payload[1])
            self.totals_val.set(payload[2])

        def _set_phenotype(payload, combo_fields):
            if self.pm_index >= len(combo_fields):
                self._add_combo_to_frame()
                combo_fields = self.combos_pane.interior.winfo_children()

            combo_fields[self.pm_index].delete(0, tk.END)
            combo_fields[self.pm_index].insert(0, payload)
            self.pm_index += 1

        def _set_marker(payload, combo_fields):
            combo_fields[self.pm_index].delete(0, tk.END)
            combo_fields[self.pm_index].insert(0, payload)
            self.pm_index += 1
        
        
        file = filedialog.askopenfilename(initialdir = "./",
                                      title = "Select Config File",
                                      filetypes = (("Config files", "*.config"),
                                                   ("all files","*.*"))
                                      )

        self.pm_index = 0
        
        if (file != "" and file.split(".")[-1] == "config"):
            with open(file, "r") as f:
                for line in f:
                    combo_fields = self.combos_pane.interior.winfo_children()
                    identifier = line.split(":")[0].strip()
                    payload = line.split(":")[1].strip()
                    result = {
                      'INPUT DIR' :       lambda x: self.input_dir_field.insert(0, x),
                      'OUTPUT DIR' :      lambda x: self.output_dir_field.insert(0, x),
                      'OUTPUT FILENAME' : lambda x: self.output_file_field.insert(0, x),
                      'MODES' :           lambda x: _set_modes(payload),
                      'PHENOTYPES' :      lambda x: _set_phenotype(payload, combo_fields),
                      'MARKERS' :         lambda x: _set_marker(payload, combo_fields)
                    }.get(identifier)(payload)
        return


    def _init_analysis_data(self):
        analysis_data["INPUT DIR"] = self.input_dir_field.get()
        analysis_data["OUTPUT DIR"] = self.output_dir_field.get()
        analysis_data["OUTPUT FILENAME"] = self.output_file_field.get()
        analysis_data["MODES"] = [int(self.mm2_val.get()), \
                                  int(self.percentages_val.get()), \
                                  int(self.totals_val.get())]
        
        combo_fields = self.combos_pane.interior.winfo_children()
        combinations = list()
        phenotypes = list()
        markers = list()
        for i in range(0, len(combo_fields), 2):
            curr_phenotypes = [x.strip() for x in combo_fields[i].get().split(",")]
            curr_markers = [x.strip() for x in combo_fields[i+1].get().split(",")]
            combinations.append((curr_phenotypes, curr_markers))
            for pheno in curr_phenotypes:
                if pheno not in phenotypes:
                    phenotypes.append(pheno)
            for marker in curr_markers:
                if marker not in markers:
                    markers.append(marker)

        analysis_data["PHENOTYPES"] = phenotypes
        analysis_data["MARKERS"] = markers
        analysis_data["COMBINATIONS"] = combinations


    def _run_analysis(self):
        self._create_analysis_file()
        self._init_analysis_data()
        analyze(analysis_data)



class VerticalScrolledFrame(ttk.Frame):
    """A pure Tkinter scrollable frame that actually works!
    * Use the 'interior' attribute to place widgets inside the scrollable frame
    * Construct and pack/place/grid normally
    * This frame only allows vertical scrolling

    """
    def __init__(self, master, *args, **kw):
        ttk.Frame.__init__(self, master, *args, **kw)            

        # create a canvas object and a vertical scrollbar for scrolling it
        self.vscrollbar = tk.Scrollbar(self, orient=tk.VERTICAL)
        self.vscrollbar.pack(fill=tk.Y, side=tk.RIGHT, expand=tk.FALSE)
        self.canvas = tk.Canvas(self, bd=0, highlightthickness=0,
                        yscrollcommand=self.vscrollbar.set)
        self.canvas.pack(side=tk.LEFT, fill=tk.BOTH, expand=tk.TRUE)
        self.vscrollbar.config(command=self.canvas.yview)

        # reset the view
        self.canvas.xview_moveto(0)
        self.canvas.yview_moveto(0)

        # create a frame inside the canvas which will be scrolled with it
        self.interior = self.interior = tk.Frame(self.canvas)
        self.interior_id = self.canvas.create_window(0, 0, window=self.interior,
                                           anchor=tk.NW)

        self.interior.bind('<Configure>', self._configure_interior)
        self.canvas.bind('<Configure>',   self._configure_canvas)

    # track changes to the canvas and frame width and sync them,
    # also updating the scrollbar
    def _configure_interior(self, event):
        # update the scrollbars to match the size of the inner frame
        size = (self.interior.winfo_reqwidth(), self.interior.winfo_reqheight())
        self.canvas.config(scrollregion="0 0 %s %s" % size)
        if self.interior.winfo_reqwidth() != self.canvas.winfo_width():
            # update the canvas's width to fit the inner frame
            self.canvas.config(width=self.interior.winfo_reqwidth())

    def _configure_canvas(self, event):
        if self.interior.winfo_reqwidth() != self.canvas.winfo_width():
            # update the inner frame's width to fill the canvas
            self.canvas.itemconfigure(self.interior_id, width=self.canvas.winfo_width())

#############################
### END CLASS DEFINITIONS ###
#############################

############################
### FUNCTION DEFINITIONS ###
############################

##############################
###### HELPER FUNCTIONS ######
##############################

def is_tumor_stroma(study_files):
    path_resolutions = [str(x).split("/")[-1].lower() for x in study_files]

    if "stroma" in path_resolutions:
        return True
    else:
        return False

##################################
###### END HELPER FUNCTIONS ######
##################################

################################
###### ANALYSIS FUNCTIONS ######
################################
def analyze_study(study_files, ts):
    print(ts)


def analyze(analysis_data):
    pprint(analysis_data)

    root_path = analysis_data["INPUT DIR"]
    studies = list(Path(root_path).glob("*"))

    for study_path in studies:
        print(study_path)
        study_files = list(Path(study_path).glob("*"))
        print(study_files)

        if is_tumor_stroma(study_files):
            analyze_study(study_files, True)
        else:
            analyze_study(study_files, False)






# # Check if file already exists
#         path = Path("./" + self.fname)
#         # (Over)write to file
#         with path.open(mode = "w") as f:
#             f.write(str_buf)

            
# Map of data needed to perform analysis
analysis_data = dict()





if __name__ == "__main__":
    # Setup graphics handler and launch main loop
    root_context = Tk()
    root_context.resizable(width = False, height = False)
    analysis_window = AnalysisWindow(root_context)
    root_context.mainloop()