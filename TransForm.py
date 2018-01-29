#! /usr/bin/env python3

from tkinter import Tk, filedialog, IntVar, Text
from tkinter import ttk
import tkinter as tk

from pathlib import Path
import os
import fnmatch as fnm
import re
import csv

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

        if (file != () and file.split(".")[-1] == "config"):
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
                if pheno not in phenotypes and pheno is not "":
                    phenotypes.append(pheno)
            for marker in curr_markers:
                if marker not in markers and marker is not "":
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
def has_subdir(files, target_dir_name):
    str_paths = [str(x) for x in files]
    target_paths = [x for x in str_paths if target_dir_name in x.lower()]
    return (len(target_paths) > 0, target_paths)


def is_tumor_stroma(case_files):
    has_tumor, tumor_paths = has_subdir(files = case_files, 
                                        target_dir_name = 'tumor')
    has_stroma, stroma_paths = has_subdir(files = case_files, 
                                          target_dir_name = 'stroma')

    return (has_tumor or has_stroma,
            tumor_paths,
            stroma_paths)


def get_img_numbers(case_files):
    img_num_pattern = ".*_(\d+|\[\d+,\d+\])_[a-zA-Z_\.]+"
    summary_pattern = "_cell_seg_data_summary.txt"

    str_paths = [str(x) for x in case_files]
    # Get all summary files in directory
    summary_paths = [x for x in str_paths if re.search(summary_pattern, x) is not None]

    # Extract image numbers from summary data paths
    img_nums = [re.search(img_num_pattern, x).group(1) for x in summary_paths \
             if re.search(img_num_pattern, x) is not None]

    return img_nums


def get_marker_thresholds(threshold_path, markers):
    thresholds = dict()
    with open(threshold_path, newline = "") as score_file:
        score_reader = csv.reader(score_file, delimiter = "\t")
        headers = next(score_reader)
        for row in score_reader:
            for marker in analysis_data["MARKERS"]:
                marker_threshold = None
                # Single marker cases produce thresholds with this title rather
                # than using the marker name.
                if "Positivity Threshold" in headers:
                    marker_threshold = row[headers.index("Positivity Threshold")]
                else:
                    r = re.compile(marker + " .*Threshold")
                    # TODO: Error possible, out of range.
                    marker_threshold = row[headers.index(list(filter(r.match, headers))[0])]
                thresholds[marker.lower()] = marker_threshold
    return thresholds


def get_marker_regions(region_path):
    region_info = { "MEMBRANE" : list(),
                    "NUCLEUS"  : list() }
    cell_compartments = list()
    stain_components = list()

    cell_pattern = ".*Cell Compartment"
    stain_pattern = ".*Stain Component"

    with open(region_path, newline = "") as score_file:
        score_reader = csv.reader(score_file, delimiter = "\t")
        headers = next(score_reader)
        r_cell = re.compile(cell_pattern)
        r_stain = re.compile(stain_pattern)
        for row in score_reader:
            # Get match object for each pattern, then iterate the matchs adding to lists.
            cell_compartment_matches = list(filter(r_cell.match, headers))
            stain_component_matches = list(filter(r_stain.match, headers))
            
            cell_compartment_list = [row[headers.index(x)] for x in cell_compartment_matches]
            stain_component_list  = [row[headers.index(x)] for x in stain_component_matches]

            cell_compartment_list = [x.split()[0].lower() for x in cell_compartment_list]
            stain_component_list  = [x.split()[0].lower() for x in stain_component_list]

            for i in range(len(cell_compartment_list)):
                if cell_compartment_list[i].lower() == "membrane":
                    region_info["MEMBRANE"].append(stain_component_list[i])
                else:
                    region_info["NUCLEUS"].append(stain_component_list[i])

    return region_info


def get_positive_sample_counts(markers, marker_scores, thresholds):
    pass
    

def compute_mm2(analysis_data, cell_summary_data, ts = False):
    pass


def compute_percentages(analysis_data, cell_summary_data, ts = False):
    pass
##################################
###### END HELPER FUNCTIONS ######
##################################

################################
###### ANALYSIS FUNCTIONS ######
################################
def run_analysis_modes(selected_modes, analysis_data, cell_summary_data, ts = False):
    pass


def count_combo_positives(cell_data, pm_combinations, thresholds):
    pass


def analyze_case(case_path, case_files, ts):   
    # Get Case numbers from study
    img_nums = sorted(get_img_numbers(case_files))

    print("IMG NUMS")
    pprint(img_nums)

    subpath_pattern = "(_)(\d+|\[\d+,\d+\])_[a-zA-Z_\.]+"
    case_subpath = re.sub(subpath_pattern, 
                          "\g<1>", 
                          str(case_files[0]).split("/")[-1])

    print("CASE SUBPATH")
    print(case_subpath)

    # Get marker positive thresholds
    threshold_path = case_path + "/" + \
                     case_subpath + img_nums[0] + "_score_data.txt"
    thresholds = get_marker_thresholds(threshold_path, analysis_data["MARKERS"])
    print("THRESHOLDS")
    pprint(thresholds)

    # Get the tissue regions for each marker
    region_info = get_marker_regions(threshold_path)

    print("REGION INFO")
    pprint(region_info)
    
    # Iterate Case numbers
    for img_num in img_nums:
        print("IMAGE NUMBER")
        print(img_num)
        continue
    #   Get cell data set
    #   if ts
    #     for each tissue region
    #       Get positive marker count
    #       Get cell data summary
    #       Run analysis mode (MM2, Percentages, ...)
    #       Append to running data sheet (Front, Master, Final, ...)
    #   else
    #     Get positive marker count
    #     Get cell data summary
    #     Run analysis mode (MM2, Percentages, ...)
    #     Append to running data sheet (Front, Master, Final, ...)
    #   Write final data frame for case to file.


def analyze(analysis_data):
    print("ANALYSIS DATA")
    pprint(analysis_data)

    root_path = analysis_data["INPUT DIR"]
    cases = list(Path(root_path).glob("*"))

    for case_path in cases:
        print("CASE PATH")
        print(case_path)
        case_files = list(Path(case_path).glob("*"))

        is_ts = is_tumor_stroma(case_files)
        tumor_paths = is_ts[1]
        stroma_paths = is_ts[2]

        print("IS TUMOR / STROMA")
        pprint(is_ts)

        if is_ts[0]:
            if len(stroma_paths) > 0:
                case_path = stroma_paths[0]
            elif len(tumor_paths) > 0:
                case_path = tumor_paths[0]
            else:
                print("Saw Tumor/Stroma in" + \
                      case_path + \
                      ", but not directory could be found")
                continue
            
            case_files = list(Path(case_path).glob("*"))
            analyze_case(case_path, case_files, True)
        else:
            analyze_case(case_path, case_files, False)
        break





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
