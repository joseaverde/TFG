import enum
import os

# ==== Structure ============================================================ #

class Language(enum.Enum):
    Ada = 1
    CXX = 2

class Real(enum.Enum):
    Fixed  = 1
    Single = 2
    Double = 3

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
    Test        = 3
    Module      = 4

# ==== Configuration ======================================================== #

class Configuration:
    def __init__ (self, *,
                  language : Language,
                  real     : Real,
                  profile  : Profile,
                  target   : Target,
                  compiler : Compiler):
        self.language = language
        self.real     = real
        self.profile  = profile
        self.target   = target
        self.compiler = compiler

    @property
    def name(self) -> str:
        return (f"{self.language.name}-{self.real.name}-{self.profile.name}-"
                f"{self.target.name}-{self.compiler.name}")

    def __str__(self) -> str:
        return self.name

    def __repr__(self) -> str:
        return self.name


# ==== Project ============================================================== #


class BaseProject:
    def __init__ (self, config):
        self.config = config

    @property
    def path(self) -> str:
        return os.path.join(BUILD_DIR, self.config.name)

    def prepare(self):
        pass

    def compile(self):
        pass

    def run(self):
        pass


class ESPProject (BaseProject):
    def prepare(self):
        pass

    def compile(self):
        pass

    def run(self):
        pass

"""
    def prepare(self):
        if not os.path.exists(self.path):
            os.makedirs(self.path)

    def __compile_ada(self):
        config = {}
        flags = []
        match self.real:
            case Real.Fixed:
                path = "fixed"
            case Real.Single:
                path = "float"
                flags.append("-XFloat_Type=Single")
            case Real.Double:
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
"""

def generate(conf : Configuration):
    if conf.language == Language.Ada:
        pass
    elif conf.language == Language.CXX:
        pass
    else:
        raise Exception(f"Invalid language {conf}")

if __name__ == "__main__":
    BUILD = "build"
    if not os.path.exists(BUILD):
        os.makedirs(BUILD);

    conf = Configuration(
            language = Language.Ada,
            real     = Real.Fixed,
            profile  = Profile.Release,
            target   = Target.Native,
            compiler = Compiler.GCC)
    generate(conf)

    print(conf.name)
