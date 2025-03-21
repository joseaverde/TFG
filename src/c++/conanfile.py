from conan import ConanFile
from conan.tools.cmake import cmake_layout

class SeizureDetector(ConanFile):
    settings = "os", "compiler", "build_type", "arch"
    generators = "CMakeToolchain", "CMakeDeps"

    def requirements(self):
        self.requires("ms-gsl/4.0.0")
        self.requires("fftw/3.3.10")
        self.requires("rapidcsv/8.80")
        self.requires("benchmark/1.8.3")
        self.requires("range-v3/0.12.0")
        self.requires("nlohmann_json/3.11.3")
        self.requires("onetbb/2021.12.0")
        self.requires("boost/1.86.0")
        self.requires("pybind11/2.10.4")

    def build_requirements(self):
        self.tool_requires("cmake/3.22.6")

    def layout(self):
        cmake_layout(self)

































"""
class CompressorRecipe(ConanFile):
    settings = "os", "compiler", "build_type", "arch"
    generators = "CMakeToolchain", "CMakeDeps"

    def requirements(self):
        self.requires("ms-gsl/4.0.0")
        self.requires("fftw/3.3.10")
        self.requires("rapidcsv/8.80")
        self.requires("benchmark/1.8.3")
        self.requires("range-v3/0.12.0")
        self.requires("nlohmann_json/3.11.3")
        self.requires("onetbb/2021.12.0")
        self.requires("boost/1.86.0")
        # if self.settings.os == "Windows":
        #     self.requires("base64/0.4.0")

    def build_requirements(self):
        if self.settings.os != "Windows":
            self.tool_requires("cmake/3.22.6")

    def layout(self):
        # We make the assumption that if the compiler is msvc the
        # CMake generator is multi-config
        multi = True if self.settings.get_safe("compiler") == "msvc" else False
        if multi:
            self.folders.generators = os.path.join("build", "generators")
            self.folders.build = "build"
        else:
            self.folders.generators = os.path.join("build", str(self.settings.build_type), "generators")
            self.folders.build = os.path.join("build", str(self.settings.build_type))
"""
