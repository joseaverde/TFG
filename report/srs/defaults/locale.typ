//! Localization support


#let SUPPORTED-LANGUAGES = ("en", "es")


#let TEMPLATE = (
  en: (name, root-class) => {
    if root-class == name {
      ["#name" template]
    } else ["#name" #root-class template]
  },
  es: (name, root-class) => {
    if root-class == name {
      [Plantilla de #root-class "#name"]
    } else [Plantilla de "#name"]
  },
)

#let FIELD = (
  en: "Field",
  es: "Campo",
)

#let DESCRIPTION = (
  en: "Description",
  es: "Descripción",
)
