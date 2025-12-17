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
    return el.closest("[tool-name][request-id]") ? false : true
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
