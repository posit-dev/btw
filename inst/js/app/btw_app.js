import { CountUp } from "./countUp/countUp.min.js"

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

// Open File Buttons ----------------------------------------------------------
document.addEventListener(
  "click",
  function (e) {
    const btn = e.target.closest(".btw-open-file")
    if (!btn) return

    const path = btn.getAttribute("data-path")
    if (!path) return

    e.stopPropagation()
    e.preventDefault()
    Shiny.setInputValue("__btw_ide_open_file", path, { priority: "event" })
  },
  { capture: true },
)

// Code Action Buttons --------------------------------------------------------
const inIframe = window.self !== window.top
const inIDE = document.querySelector(".btw-in-ide") !== null

if (inIframe && inIDE) {
  const stopObserving = observeShinyMarkdownStream((streamEl) => {
    enhanceCodeActions(streamEl)
  })

  function isMarkdownStream(el) {
    return el.matches("shiny-markdown-stream")
  }

  function canEnhance(el) {
    return el.closest("[tool-name][request-id], [tool-title][request-id]")
      ? el.closest(".btw-output-source")
        ? true
        : false
      : true
  }

  function observeShinyMarkdownStream(callback, delay = 200) {
    const observers = new Map()

    function attachObserver(el) {
      if (!isMarkdownStream(el) || observers.has(el)) return

      let timeoutId = null

      const observer = new MutationObserver(() => {
        clearTimeout(timeoutId)
        timeoutId = setTimeout(() => callback(el), delay)
      })

      observer.observe(el, {
        childList: true,
        subtree: true,
        characterData: true,
      })

      observers.set(el, observer)
    }

    document.querySelectorAll("shiny-markdown-stream").forEach(attachObserver)

    const rootObserver = new MutationObserver((mutations) => {
      mutations.forEach((mutation) => {
        mutation.addedNodes.forEach((node) => {
          if (node.nodeType !== Node.ELEMENT_NODE) return

          if (isMarkdownStream(node)) {
            attachObserver(node)
          }

          node
            .querySelectorAll?.("shiny-markdown-stream")
            .forEach(attachObserver)
        })
      })
    })

    rootObserver.observe(document.documentElement, {
      childList: true,
      subtree: true,
    })

    return () => {
      rootObserver.disconnect()
      observers.forEach((observer) => observer.disconnect())
      observers.clear()
    }
  }

  function enhanceCodeActions(root) {
    root.querySelectorAll(".code-copy-button").forEach((copyButton) => {
      const pre = copyButton.closest("pre")
      if (!pre || !canEnhance(pre)) return

      const wrapper = ensureWrapper(pre)
      moveCopyButton(wrapper, copyButton)
      installActionButtons(wrapper, pre)
    })
  }

  function ensureWrapper(pre) {
    let wrapper = pre.querySelector(".code-action-wrapper")
    if (wrapper) return wrapper

    wrapper = document.createElement("div")
    wrapper.className = "code-action-wrapper"
    pre.appendChild(wrapper)
    return wrapper
  }

  function moveCopyButton(wrapper, copyButton) {
    if (copyButton.parentElement !== wrapper) {
      wrapper.appendChild(copyButton)
    }
  }

  function extractLanguage(pre) {
    const code = pre.querySelector("code")
    if (!code) return null

    const langClass = [...code.classList].find((cls) =>
      cls.startsWith("language-"),
    )
    return langClass ? langClass.replace("language-", "") : null
  }

  function installActionButtons(wrapper, pre) {
    const actions = [
      {
        selector: ".insert-at-cursor-button",
        className: "insert-at-cursor-button",
        label: "Insert code at cursor",
        payload: () => ({
          location: "cursor",
          code: pre.innerText,
        }),
      },
      {
        selector: ".insert-new-file-button",
        className: "insert-new-file-button",
        label: "Insert code in new file",
        payload: () => ({
          location: "new_file",
          code: pre.innerText,
          language: extractLanguage(pre),
        }),
      },
      {
        selector: ".insert-console-button",
        className: "insert-console-button",
        label: "Run code in console",
        payload: () => ({
          location: "console",
          code: pre.innerText,
        }),
      },
    ]

    actions.forEach((action) => {
      if (wrapper.querySelector(action.selector)) return

      const button = document.createElement("button")
      button.type = "button"
      button.className = action.className
      button.ariaLabel = action.label
      button.title = action.label
      button.innerHTML = '<i class="bi" aria-hidden="true"></i>'

      button.addEventListener("click", (event) => {
        event.stopPropagation()
        Shiny.setInputValue("__btw_ide_insert_code", action.payload(), {
          priority: "event",
        })
      })

      wrapper.appendChild(button)
    })
  }
}

// Keyboard Shortcuts ---------------------------------------------------------
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
