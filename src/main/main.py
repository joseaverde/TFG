import enum

class Strategy(enum.Enum):
    Fixed = 1
    Single = 2
    Double = 3

class Language(enum.Enum):
    Ada = 1
    CXX = 2

class Profile(enum.Enum):
    Release           = 1
    ReleaseWithChecks = 2
    Validation        = 3
    Development       = 4

class Target(enum.Enum):
    Native       = 1
    RaspberryPi3 = 2
    RaspberryPi4 = 3
    ESP32C3      = 4
    ESP32C6      = 5
    ESP32S3      = 6

class Compiler(enum.Enum):
    GCC   = 1
    Clang = 2

class Executable(enum.Enum):
    Benchmark   = 1
    Application = 2

class Configuration:
    def __init__ (self,
                  language : Language,
                  strategy : Strategy,
                  profile  : Profile,
                  target   : Target,
                  compiler : Compiler):
        self.language = language
        self.strategy = strategy
        self.profile  = profile
        self.target   = target
        self.compiler = compiler

    def __compile_ada(self):
        config = {}
        flags = []
        match self.strategy:
            case Strategy.Fixed:
                path = "fixed"
            case Strategy.Single:
                path = "float"
                flags.append("-XFloat_Type=Single")
            case Strategy.Double:
                path = "float"
                flags.append("-XFloat_Type=Double")

        is_esp32 = False
        match self.target:
            case Target.Native:
                target = "native"
            case Target.RaspberryPi3:
                target = "rpi3"
            case Target.RaspberryPi4:
                target = "rpi4"
            case Target.ESP32C3:
                target = "esp32c3"
                is_esp32 = True
            case Target.ESP32C6:
                target = "esp32c6"
                is_esp32 = True
            case Target.ESP32S3:
                target = "esp32s3"
                is_esp32 = True

        match self.profile:
            case Profile.Release:
                flags.append("--release")
                flags.append("-XDisable_Checks=True")
            case Profile.ReleaseWithChecks:
                flags.append("--release")
            case Profile.Validation:
                flags.append("--validation")
            case Profile.Development:
                flags.append("--development")

        path = os.path.join(root, "ada", path)

    def compile(self):
        if self.language == Language.Ada:
            self.__compile_ada()
        else:
            self.__compile_cxx()
