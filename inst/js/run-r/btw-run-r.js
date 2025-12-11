/**
 * Custom element for displaying btw_tool_run_r results in shinychat.
 * @module btw-run-r
 */

// Ensure shinychat's hidden requests set exists
window.shinychat = window.shinychat || {}
window.shinychat.hiddenToolRequests =
  window.shinychat.hiddenToolRequests || new Set()

/**
 * SVG icons used in the component
 */
const ICONS = {
  code: `<svg xmlns="http://www.w3.org/2000/svg" width="16" height="16" fill="currentColor" class="bi bi-code-slash" viewBox="0 0 16 16">
  <path d="M10.478 1.647a.5.5 0 1 0-.956-.294l-4 13a.5.5 0 0 0 .956.294zM4.854 4.146a.5.5 0 0 1 0 .708L1.707 8l3.147 3.146a.5.5 0 0 1-.708.708l-3.5-3.5a.5.5 0 0 1 0-.708l3.5-3.5a.5.5 0 0 1 .708 0m6.292 0a.5.5 0 0 0 0 .708L14.293 8l-3.147 3.146a.5.5 0 0 0 .708.708l3.5-3.5a.5.5 0 0 0 0-.708l-3.5-3.5a.5.5 0 0 0-.708 0"/>
</svg>`,
  playCircle: `<svg xmlns="http://www.w3.org/2000/svg" height="24px" viewBox="0 -960 960 960" width="24px" fill="currentColor"><path d="m380-300 280-180-280-180v360ZM480-80q-83 0-156-31.5T197-197q-54-54-85.5-127T80-480q0-83 31.5-156T197-763q54-54 127-85.5T480-880q83 0 156 31.5T763-763q54 54 85.5 127T880-480q0 83-31.5 156T763-197q-54 54-127 85.5T480-80Zm0-80q134 0 227-93t93-227q0-134-93-227t-227-93q-134 0-227 93t-93 227q0 134 93 227t227 93Zm0-320Z"/></svg>`,
  exclamationCircleFill: `<svg xmlns="http://www.w3.org/2000/svg" width="16" height="16" fill="currentColor" class="bi bi-exclamation-circle-fill" viewBox="0 0 16 16">
  <path d="M16 8A8 8 0 1 1 0 8a8 8 0 0 1 16 0M8 4a.905.905 0 0 0-.9.995l.35 3.507a.552.552 0 0 0 1.1 0l.35-3.507A.905.905 0 0 0 8 4m.002 6a1 1 0 1 0 0 2 1 1 0 0 0 0-2"/>
</svg>`,
  plus: `<svg xmlns="http://www.w3.org/2000/svg" width="10px" height="10px" viewBox="4 4 12 12" fill="none">
  <path class="horizontal" d="M5 11C4.44772 11 4 10.5523 4 10C4 9.44772 4.44772 9 5 9H15C15.5523 9 16 9.44772 16 10C16 10.5523 15.5523 11 15 11H5Z" fill="currentColor"/>
  <path class="vertical" d="M9 5C9 4.44772 9.44772 4 10 4C10.5523 4 11 4.44772 11 5V15C11 15.5523 10.5523 16 10 16C9.44772 16 9 15.5523 9 15V5Z" fill="currentColor"/>
</svg>`,
  copy: `<svg xmlns="http://www.w3.org/2000/svg" width="16" height="16" fill="currentColor" class="bi bi-clipboard" viewBox="0 0 16 16">
  <path d="M4 1.5H3a2 2 0 0 0-2 2V14a2 2 0 0 0 2 2h10a2 2 0 0 0 2-2V3.5a2 2 0 0 0-2-2h-1v1h1a1 1 0 0 1 1 1V14a1 1 0 0 1-1 1H3a1 1 0 0 1-1-1V3.5a1 1 0 0 1 1-1h1z"/>
  <path d="M9.5 1a.5.5 0 0 1 .5.5v1a.5.5 0 0 1-.5.5h-3a.5.5 0 0 1-.5-.5v-1a.5.5 0 0 1 .5-.5zm-3-1A1.5 1.5 0 0 0 5 1.5v1A1.5 1.5 0 0 0 6.5 4h3A1.5 1.5 0 0 0 11 2.5v-1A1.5 1.5 0 0 0 9.5 0z"/>
</svg>`,
}

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
      this.icon = ICONS.playCircle
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
      await navigator.clipboard.writeText(reprexOutput)

      // Get the tooltip instance
      const tooltip = window.bootstrap?.Tooltip?.getInstance(copyBtn)

      // Visual feedback - change icon briefly and update tooltip
      copyBtn.innerHTML = `<svg xmlns="http://www.w3.org/2000/svg" width="16" height="16" fill="currentColor" class="bi bi-check" viewBox="0 0 16 16">
        <path d="M10.97 4.97a.75.75 0 0 1 1.07 1.05l-3.99 4.99a.75.75 0 0 1-1.08.02L4.324 8.384a.75.75 0 1 1 1.06-1.06l2.094 2.093 3.473-4.425z"/>
      </svg>`

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

if (!customElements.get("btw-run-r-result")) {
  customElements.define("btw-run-r-result", BtwRunRResult)
}
