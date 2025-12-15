/**
 * Custom element for displaying btw_tool_run_r results in shinychat.
 * @module btw-run-r
 */

import { ICONS } from "./btw-icons.js"

// Ensure shinychat's hidden requests set exists
window.shinychat = window.shinychat || {}
window.shinychat.hiddenToolRequests =
  window.shinychat.hiddenToolRequests || new Set()

/**
 * Formats code as a Markdown code block for rendering.
 * @param {string} content - The code content
 * @param {string} [language="r"] - The language for syntax highlighting
 * @returns {string} Markdown code block
 */
function markdownCodeBlock(content, language = "r") {
  const backticks = "`".repeat(8)
  return `${backticks}${language}\n${content}\n${backticks}`
}

/**
 * Web component that displays the result of btw_tool_run_r execution.
 *
 * @element btw-run-r-result
 * @attr {string} request-id - Unique identifier linking to the tool request
 * @attr {string} code - The R code that was executed
 * @attr {string} status - Execution status: "success" or "error"
 *
 * @example
 * <btw-run-r-result
 *   request-id="abc123"
 *   code="1 + 1"
 *   status="success"
 * >
 *   <pre><code>[1] 2</code></pre>
 * </btw-run-r-result>
 */
class BtwRunRResult extends HTMLElement {
  /** @type {boolean} */
  expanded = true

  constructor() {
    super()

    this.toolTitle = this.getAttribute("tool-title") || "Run R Code"
    this.icon = this.getAttribute("icon") || ICONS.playCircle
  }

  connectedCallback() {
    // Set status-based styling
    const status = this.getAttribute("status")
    if (status === "error") {
      this.classStatus = "text-danger"
      this.icon = ICONS.exclamationCircleFill
      this.titleTemplate = "{title} failed"
    } else {
      this.classStatus = ""
      this.titleTemplate = "{title}"
    }

    // Hide the corresponding tool request
    const requestId = this.getAttribute("request-id")
    if (requestId) {
      // TODO: Remove after next shinychat release (posit-dev/shinychat#163)
      window.shinychat.hiddenToolRequests.add(requestId)
      this.dispatchEvent(
        new CustomEvent("shiny-tool-request-hide", {
          detail: { request_id: requestId },
          bubbles: true,
          cancelable: true,
        }),
      )
    }

    this.render()

    // Signal that chat may need to scroll
    this.dispatchEvent(new CustomEvent("shiny-chat-maybe-scroll-to-bottom"))
  }

  disconnectedCallback() {
    // Clean up tooltip when component is removed from DOM
    const copyBtn = this.querySelector(".copy-code-btn")
    if (copyBtn) {
      const tooltip = window.bootstrap?.Tooltip?.getInstance(copyBtn)
      if (tooltip) {
        tooltip.dispose()
      }
    }
  }

  /**
   * Toggle the collapsed/expanded state
   * @param {Event} e
   */
  toggleCollapse(e) {
    e.preventDefault()
    this.expanded = !this.expanded
    this.render()
  }

  /**
   * Generate reprex-style output from the code and results
   * @returns {string} Formatted reprex output
   */
  generateReprexOutput() {
    const outputContainer = this.querySelector(".btw-run-output")
    if (!outputContainer) {
      return this.getAttribute("code") || ""
    }

    const parts = []
    const preElements = outputContainer.querySelectorAll("pre")

    preElements.forEach((pre) => {
      // Skip if this is inside an image or other non-text content
      if (pre.closest("img")) {
        return
      }

      // Get the text content
      const code = pre.querySelector("code")
      const text = code ? code.textContent : pre.textContent

      if (!text.trim()) {
        return
      }

      // Source code is added as-is
      if (pre.classList.contains("btw-output-source")) {
        parts.push(text.trimEnd())
      }
      // Other outputs get #> prefix on each line
      else if (
        pre.classList.contains("btw-output-output") ||
        pre.classList.contains("btw-output-message") ||
        pre.classList.contains("btw-output-warning") ||
        pre.classList.contains("btw-output-error")
      ) {
        const lines = text.trimEnd().split("\n")
        const prefixed = lines.map((line) => "#> " + line).join("\n")
        parts.push(prefixed)
      }
    })

    return parts.join("\n")
  }

  /**
   * Copy code to clipboard
   * @param {Event} e
   */
  async copyCode(e) {
    e.preventDefault()
    e.stopPropagation() // Prevent triggering collapse toggle

    // Save reference to button before async operation
    // (e.currentTarget becomes null after await)
    const copyBtn = e.currentTarget

    try {
      const originalHtml = copyBtn.innerHTML
      const reprexOutput = this.generateReprexOutput()
      await copyToClipboard(reprexOutput)

      // Get the tooltip instance
      const tooltip = window.bootstrap?.Tooltip?.getInstance(copyBtn)

      // Visual feedback - change icon briefly and update tooltip
      copyBtn.innerHTML = ICONS.check

      // Update tooltip to show success message
      if (tooltip) {
        const originalTitle = copyBtn.getAttribute("data-bs-original-title")
        copyBtn.setAttribute("data-bs-original-title", "Copied code!")
        tooltip.setContent({ ".tooltip-inner": "Copied code!" })
        if (copyBtn.matches(":hover")) {
          tooltip.show()
        }

        setTimeout(() => {
          copyBtn.innerHTML = originalHtml
          copyBtn.setAttribute("data-bs-original-title", originalTitle || "Copy source code")
          tooltip.setContent({
            ".tooltip-inner": originalTitle || "Copy source code",
          })
          tooltip.hide()
        }, 1500)
      } else {
        setTimeout(() => {
          copyBtn.innerHTML = originalHtml
        }, 1500)
      }
    } catch (err) {
      console.error("Failed to copy code:", err)
    }
  }

  /**
   * Formats the title for display in the card header. Uses the `titleTemplate`,
   * replacing `{title}` with the actual title or name of the tool.
   * @returns {string}
   */
  formatTitle() {
    const displayTitle = `<span class="tool-title-name">${
      this.toolTitle || "Run R Code"
    }</span>`
    return this.titleTemplate.replace("{title}", displayTitle)
  }

  /**
   * Render the component
   */
  render() {
    const requestId = this.getAttribute("request-id") || "unknown"
    const code = this.getAttribute("code") || ""
    const headerId = `tool-header-${requestId}`
    const contentId = `tool-content-${requestId}`

    // Get the output HTML from child content (set during initial render)
    const outputHtml = this._outputHtml || this.innerHTML
    this._outputHtml = outputHtml

    const collapsedClass = this.expanded ? "" : " collapsed"

    // Dispose of existing tooltip before re-rendering
    const oldCopyBtn = this.querySelector(".copy-code-btn")
    if (oldCopyBtn) {
      const oldTooltip = window.bootstrap?.Tooltip?.getInstance(oldCopyBtn)
      if (oldTooltip) {
        oldTooltip.dispose()
      }
    }

    this.innerHTML = `
      <div class="shiny-tool-card card bslib-card html-fill-item html-fill-container m-0">
        <div class="card-header" id="${headerId}">
          <div class="tool-icon ${this.classStatus}">${this.icon}</div>
          <div class="tool-title ${
            this.classStatus
          }">${this.formatTitle()}</div>
          <div class="tool-spacer"></div>
          <button
            class="copy-code-btn"
            aria-label="Copy code to clipboard"
            data-bs-toggle="tooltip"
            data-bs-placement="top"
            data-bs-title="Copy source code"
          >
            ${ICONS.copy}
          </button>
          <button
            class="collapse-toggle-btn"
            aria-expanded="${this.expanded}"
            aria-controls="${contentId}"
            aria-label="${this.expanded ? "Collapse" : "Expand"} tool output"
          >
            <div class="collapse-indicator">${ICONS.plus}</div>
          </button>
        </div>
        <div
          class="card-body bslib-gap-spacing html-fill-item html-fill-container${collapsedClass}"
          id="${contentId}"
          role="region"
          aria-labelledby="${headerId}"
          ${!this.expanded ? 'inert=""' : ""}
        >
          <div class="btw-run-output">
            ${outputHtml}
          </div>
        </div>
      </div>
    `

    const collapseBtn = this.querySelector(".collapse-toggle-btn")
    if (collapseBtn) {
      collapseBtn.addEventListener("click", (e) => this.toggleCollapse(e))
    }

    const copyBtn = this.querySelector(".copy-code-btn")
    if (copyBtn) {
      copyBtn.addEventListener("click", (e) => this.copyCode(e))

      // Initialize Bootstrap tooltip
      if (window.bootstrap?.Tooltip) {
        new window.bootstrap.Tooltip(copyBtn)
      }
    }

    // Allow clicking anywhere on the header to toggle, except on action buttons
    const header = this.querySelector(".card-header")
    if (header) {
      header.addEventListener("click", (e) => {
        // Don't toggle if clicking on a button
        if (e.target.closest(".copy-code-btn") || e.target.closest(".collapse-toggle-btn")) {
          return
        }
        this.toggleCollapse(e)
      })
    }
  }

  /**
   * Escape a string for use in an HTML attribute
   * @param {string} str
   * @returns {string}
   */
  escapeAttr(str) {
    return str
      .replace(/&/g, "&amp;")
      .replace(/"/g, "&quot;")
      .replace(/'/g, "&#39;")
      .replace(/</g, "&lt;")
      .replace(/>/g, "&gt;")
  }
}

/**
 * Copy text to clipboard with fallback for older browsers
 * @param {string} text - The text to copy
 * @returns {Promise<void>}
 */
function copyToClipboard(text) {
  if (window.isSecureContext && navigator.clipboard) {
    return navigator.clipboard.writeText(text).catch(() => fallbackCopy(text))
  } else {
    return fallbackCopy(text)
  }
}

/**
 * Fallback clipboard copy using document.execCommand
 * @param {string} text - The text to copy
 * @returns {Promise<void>}
 */
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
      const successful = document.execCommand("copy")
      document.body.removeChild(textArea)
      if (successful) {
        resolve()
      } else {
        throw new Error("execCommand copy failed")
      }
    } catch (err) {
      document.body.removeChild(textArea)
      window.dispatchEvent(
        new CustomEvent("shiny:client-message", {
          detail: {
            headline: "Could not copy text",
            message: "Unfortunately, this browser does not support copying to the clipboard automatically. Please copy the text manually.",
            status: "warning"
          },
        }),
      )
      reject(err)
    }
  })
}

if (!customElements.get("btw-run-r-result")) {
  customElements.define("btw-run-r-result", BtwRunRResult)
}
