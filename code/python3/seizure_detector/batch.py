class Batch:
    psd_1    : tuple[float, float] = (-np.inf, np.inf)
    psd_2    : tuple[float, float] = (-np.inf, np.inf)
    psd_3    : tuple[float, float] = (-np.inf, np.inf)
    max_dist : tuple[float, float] = (-np.inf, np.inf)
    energy   : tuple[float, float] = (-np.inf, np.inf)
    d_max_c  : float               = np.inf
    pj       : Channel
    lids     : list[int]           = [] # Pattern indices
    def __init__(self, Scv : Channel, patient : str, json_file : str):
        with open(json_file, "r") as fp:
            data = json.load(fp)
        self.psd_1 = (data[patient]["p1_min"], data[patient]["p1_max"])
        self.psd_2 = (data[patient]["p2_min"], data[patient]["p2_max"])
        self.psd_3 = (data[patient]["p3_min"], data[patient]["p3_max"])
        self.max_dist = (data[patient]["d_min"], data[patient]["d_max"])
        self.energy = (data[patient]["e_min"], data[patient]["e_max"])
        self.d_max_c = data[patient]["dmax"]
        index = int(data[patient]["batch"]) * stride
        self.pj = Scv[index : index + query_size]
        self.lids = np.array([index])

    def __repr__ (self):
        return ( f"Batch [\n"
               + f"   PSD (1)  : {self.psd_1}\n"
               + f"   PSD (2)  : {self.psd_2}\n"
               + f"   PSD (3)  : {self.psd_3}\n"
               + f"   Distance : {self.max_dist}\n"
               + f"   Energy   : {self.energy}\n"
               + f"   D_max_c  : {self.d_max_c}\n"
               + f"];")
