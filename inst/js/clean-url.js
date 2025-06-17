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
      ".post-content",
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

  /**
   * Replaces all <iframe> elements with their body HTML,
   * converting any relative links/images to absolute URLs.
   */
  function inlineIframes(element) {
    // Convert NodeList to Array for safe iteration if we remove/reorder elements
    const iframes = Array.from(element.getElementsByTagName("iframe"));

    iframes.forEach((iframe) => {
      let iframeDoc;
      try {
        // Try to access the iframe's document (may fail cross-origin)
        iframeDoc = iframe.contentDocument || iframe.contentWindow.document;
      } catch (e) {
        console.warn(
          "Cannot access iframe content (cross-origin?):",
          iframe,
          e
        );
        iframe.remove();
        return;
      }

      if (!iframeDoc || !iframeDoc.body) {
        console.warn("No body found in iframe:", iframe);
        iframe.remove();
        return;
      }

      // Grab the raw body HTML
      const rawHtml = iframeDoc.body.innerHTML;

      // Create a container to manipulate links and images
      const container = document.createElement("div");
      container.innerHTML = rawHtml;

      // Base URL for resolving relative paths
      // If the iframe has its own src, use that; otherwise fallback to page URL
      const baseUrl = iframe.src ? iframe.src : window.location.href;

      // Convert all <a href> to absolute URLs
      Array.from(container.getElementsByTagName("a")).forEach((a) => {
        const href = a.getAttribute("href");
        if (href) {
          try {
            a.setAttribute("href", new URL(href, baseUrl).href);
          } catch (e) {
            console.warn(
              "Failed to resolve href:",
              href,
              "with base",
              baseUrl,
              e
            );
          }
        }
      });

      // Convert all <img src> to absolute URLs
      Array.from(container.getElementsByTagName("img")).forEach((img) => {
        const src = img.getAttribute("src");
        if (src) {
          try {
            img.setAttribute("src", new URL(src, baseUrl).href);
          } catch (e) {
            console.warn(
              "Failed to resolve src:",
              src,
              "with base",
              baseUrl,
              e
            );
          }
        }
      });

      // Replace the iframe with the processed HTML
      // We insert the children of container so we don't nest a <div>
      const parent = iframe.parentNode;
      while (container.firstChild) {
        parent.insertBefore(container.firstChild, iframe);
      }
      parent.removeChild(iframe);
    });
  }

  function cleanContent(element) {
    // Inline all iframes
    inlineIframes(element);

    // Remove unwanted tags
    const blockList = [
      // https://github.com/cure53/DOMPurify/wiki/Default-TAGs-ATTRIBUTEs-allow-list-&-blocklist#html-tags
      "applet",
      "base",
      "basefont",
      "command",
      "embed",
      "frame",
      "frameset",
      // "iframe", // Already handled by inlineIframes
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
