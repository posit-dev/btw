import { CountUp } from "./countUp.min.js"

const statusCounters = new WeakMap()

const statusPrefixes = {
  tokens_input: "&uparrow;",
  tokens_output: "&downarrow;",
  cost: "$",
}

function initializeCountUp(element, initialValue, options) {
  const statusType = element.dataset.type
  const counter = new CountUp(element, initialValue, {
    duration: 1,
    prefix: statusPrefixes[statusType],
    separator: ",",
    decimal: ".",
    ...(options || {}),
  })
  return counter
}

document.addEventListener("DOMContentLoaded", function () {
  document.querySelectorAll(".status-countup").forEach((element) => {
    const counter = initializeCountUp(element, 0)
    statusCounters.set(element, counter)
  })
})

if (typeof Shiny !== "undefined") {
  Shiny.addCustomMessageHandler("btw_update_status", function (message) {
    const element = document.getElementById(message.id)
    if (element) {
      const counter = statusCounters.get(element)
      const lastValue = parseFloat(element.dataset.value | "0")

      if (counter && message.value) {
        if (
          element.dataset.type === "cost" &&
          (lastValue < 0.1 || message.value < 0.1)
        ) {
          const newCounter = initializeCountUp(element, lastValue, {
            decimalPlaces:
              message.value < 0.01 ? 4 : message.value < 0.1 ? 3 : 2,
          })
          statusCounters.set(element, newCounter)
          newCounter.update(message.value)
        } else {
          counter.update(message.value)
        }
      }
      element.dataset.value = message.value

      element.classList.toggle(
        "btw-status-recalculating",
        message.status === "recalculating",
      )

      if (message.status === "unknown") {
        element.classList.add("btw-status-unknown")
      }
    }
  })
}

function isInInput(el) {
  if (!el) return false

  const tag = el.tagName
  if (!tag) return false

  const tagUpper = tag.toUpperCase()

  if (["INPUT", "SELECT", "TEXTAREA"].includes(tagUpper)) {
    return true
  }

  if (el.isContentEditable) {
    return true
  }

  return false
}

function toggleShortcutsModal(e) {
  const modalEl = document.getElementById("btw_keyboard_shortcuts_modal")

  if (!modalEl) {
    const isMac = navigator.platform.toUpperCase().includes("MAC")
    const platform = isMac ? "macOS" : "Windows/Linux"
    const modifierKeyLabel = isMac ? "⌘" : "Ctrl"

    const shortcuts = [
      { action: "Toggle btw tools sidebar", keys: modifierKeyLabel + "+B" },
      { action: "Focus chat input", keys: modifierKeyLabel + "+." },
      { action: "Show keyboard shortcuts", keys: "?" },
    ]

    e.preventDefault()
    e.stopPropagation()
    Shiny.setInputValue(
      "__btw_show_shortcuts",
      { platform, shortcuts },
      { priority: "event" },
    )
  } else {
    const modal = modalEl.closest(".modal")
    if (modal) {
      e.preventDefault()
      e.stopPropagation()
      const bsModal = bootstrap.Modal.getInstance(modal)
      bsModal.hide()
    }
  }
}

function focusChatInput(e) {
  const chatInput = document.querySelector(
    "#chat-chat shiny-chat-input textarea",
  )
  if (chatInput) {
    if (e) {
      e.preventDefault()
      e.stopPropagation()
    }
    chatInput.focus()
  }
}

function platformCmdOrCtrl(e) {
  const isMac = navigator.platform.toUpperCase().includes("MAC")
  return isMac ? e.metaKey : e.ctrlKey
}
document.addEventListener("keydown", function (e) {
  if (isInInput(e.target)) {
    return
  }

  // Trigger only on bare "?" press (Shift + /)
  if (e.key === "?" && !e.metaKey && !e.ctrlKey && !e.altKey) {
    toggleShortcutsModal(e)
    return
  }

  // Focus chat input on "." press
  if (e.key === "." && platformCmdOrCtrl(e)) {
    focusChatInput(e)
    return
  }
})

document.addEventListener("keydown", function (e) {
  if (!platformCmdOrCtrl(e)) return
  if (e.key.toLowerCase() !== "b") return

  const el = document.getElementById("tools_sidebar")
  if (el) {
    const sidebarHadFocus = el.contains(document.activeElement)
    const sidebar = bslib.Sidebar.getInstance(el.parentElement)
    if (sidebar) {
      e.preventDefault()
      e.stopPropagation()
      sidebar.toggle()

      document.addEventListener(
        "bslib.sidebar",
        function (e) {
          if (e.detail.open) {
            el.querySelector("button").focus()
          } else if (sidebarHadFocus) {
            focusChatInput()
          }
        },
        { once: true },
      )
    }
  }
})

document.addEventListener("DOMContentLoaded", function () {
  focusChatInput()
})
