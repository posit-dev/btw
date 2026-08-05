/**
 * Progressive enhancements for rich R tool results rendered by shinychat.
 * @module btw-run-r
 */

const RUN_OUTPUT_SELECTOR = ".btw-run-output"

function outputBlocks(root) {
  const outputs = []

  if (root instanceof Element && root.matches(RUN_OUTPUT_SELECTOR)) {
    outputs.push(root)
  }

  root.querySelectorAll?.(RUN_OUTPUT_SELECTOR).forEach((output) => {
    outputs.push(output)
  })

  return outputs
}

function copyToClipboard(text) {
  if (window.isSecureContext && navigator.clipboard) {
    return navigator.clipboard.writeText(text).catch(() => fallbackCopy(text))
  }

  return fallbackCopy(text)
}

function fallbackCopy(text) {
  return new Promise((resolve, reject) => {
    const textArea = document.createElement("textarea")
    textArea.value = text
    textArea.style.position = "fixed"
    textArea.style.opacity = "0"
    document.body.appendChild(textArea)
    textArea.focus()
    textArea.select()

    try {
      if (!document.execCommand("copy")) {
        throw new Error("execCommand copy failed")
      }
      resolve()
    } catch (error) {
      reject(error)
    } finally {
      textArea.remove()
    }
  })
}

function sourceText(pre) {
  return pre.querySelector("code")?.textContent ?? pre.textContent ?? ""
}

function reprexText(output) {
  const parts = []

  output.querySelectorAll("pre").forEach((pre) => {
    const text = sourceText(pre)
    if (!text.trim()) return

    if (pre.classList.contains("btw-output-source")) {
      parts.push(text.trimEnd())
      return
    }

    const lines = text.trimEnd().split("\n")
    parts.push(lines.map((line) => "#> " + line).join("\n"))
  })

  return parts.join("\n")
}

function markCopied(button) {
  button.classList.add("code-copy-button-checked")
  window.setTimeout(() => {
    button.classList.remove("code-copy-button-checked")
  }, 1500)
}

function addCopyButton(pre) {
  if (pre.querySelector(".code-copy-button")) return

  const button = document.createElement("button")
  button.type = "button"
  button.className = "code-copy-button"
  button.setAttribute("aria-label", "Copy to clipboard")
  button.innerHTML = '<i class="bi" aria-hidden="true"></i>'
  button.addEventListener("click", async (event) => {
    event.stopPropagation()

    try {
      await copyToClipboard(sourceText(pre))
      markCopied(button)
    } catch (error) {
      console.error("Failed to copy R result block:", error)
    }
  })

  pre.appendChild(button)
}

function enhanceRunOutput(root) {
  outputBlocks(root).forEach((output) => {
    output.querySelectorAll("pre").forEach(addCopyButton)
  })
}

enhanceRunOutput(document)

const observer = new MutationObserver((mutations) => {
  mutations.forEach((mutation) => {
    mutation.addedNodes.forEach((node) => {
      if (node.nodeType === Node.ELEMENT_NODE) {
        enhanceRunOutput(node)
      }
    })
  })
})

observer.observe(document.documentElement, {
  childList: true,
  subtree: true,
})

document.addEventListener("click", async (event) => {
  const button = event.target.closest(".btw-copy-reprex")
  if (!button) return

  event.preventDefault()
  event.stopPropagation()

  const output = button
    .closest(".shiny-tool-card")
    ?.querySelector(RUN_OUTPUT_SELECTOR)
  if (!output) return

  try {
    await copyToClipboard(reprexText(output))
    markCopied(button)
  } catch (error) {
    console.error("Failed to copy reprex:", error)
  }
})
