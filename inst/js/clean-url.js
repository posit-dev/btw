(function () {
  // Function to find the main content container
  function findMainContent() {
    // List of selectors to try, in order of preference
    const selectors = [
      "main",
      '[role="main"]',
      "#main",
      ".main",
      "#content",
      ".content",
      "#main-content",
      ".main-content",
      ".container.page",
      "article",
      ".article",
      "#article",
      ".post",
      "#post",
      ".entry",
      "#entry",
      ".container",
      "#container",
    ];

    let bestCandidate = null;
    let maxTextLength = 0;

    for (const selector of selectors) {
      const candidate = document.querySelector(selector);
      if (!candidate) continue;
      const textLength = candidate.innerText.trim().length;

      // Skip if too small or if it contains the entire body content
      if (
        textLength > 200 &&
        textLength < document.body.innerText.length * 0.9
      ) {
        if (textLength > maxTextLength) {
          maxTextLength = textLength;
          bestCandidate = candidate;
        }
      }
    }

    // Fallback to body if nothing else works
    return bestCandidate || document.body;
  }

  function cleanContent(element) {
    const blockList = [
      // https://github.com/cure53/DOMPurify/wiki/Default-TAGs-ATTRIBUTEs-allow-list-&-blocklist#html-tags
      "applet",
      "base",
      "basefont",
      "command",
      "embed",
      "frame",
      "frameset",
      "iframe",
      "keygen",
      "link",
      "meta",
      "noframes",
      "noscript",
      "object",
      "param",
      "script",
      "title",
      // For Markdown conversion, we also want to remove these tags
      "style",
      "svg",
    ];

    blockList.forEach((tag) => {
      element.querySelectorAll(tag).forEach((el) => el.remove());
    });

    // Simplify `<code>` elements
    element.querySelectorAll("code").forEach((codeElement) => {
      // 1. Remove all span wrappers while preserving text content
      const spans = codeElement.querySelectorAll("span");
      spans.forEach((span) => {
        // Replace span with its text content
        const textNode = document.createTextNode(span.textContent);
        span.parentNode.replaceChild(textNode, span);
      });

      // Normalize text nodes (merge adjacent text nodes)
      codeElement.normalize();

      // 2. Check if code is multi-line and parent isn't <pre>
      const isMultiLine = codeElement.textContent.includes("\n");
      const parentIsPre = codeElement.parentElement.tagName === "PRE";

      if (!isMultiLine || parentIsPre) {
        return;
      }

      // 3. Wrap the code element in a pre element
      const preElement = document.createElement("pre");
      codeElement.parentNode.insertBefore(preElement, codeElement);
      preElement.appendChild(codeElement);
    });

    // Ensure all links are absolute
    element.querySelectorAll("a").forEach((el) => {
      if (el.href && !el.getAttribute("href").startsWith("http")) {
        el.setAttribute("href", new URL(el.href, document.location.href).href);
      }
    });

    return element;
  }

  const mainContentElement = findMainContent();
  cleanContent(mainContentElement);

  return mainContentElement.innerHTML;
})();
