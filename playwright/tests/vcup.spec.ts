// @ts-check
import { test, expect } from "@playwright/test";

test.beforeEach(async ({ page }) => {
  await page.goto("/");
});

test("vcup-root", async ({ page }) => {
  const vcupRoot = page.getByTestId("vcup-root");
  await expect(vcupRoot).toMatchAriaSnapshot(`
    - article:
      - heading "Text nodes" [level=2]
      - heading "Text" [level=3]
      - text: Hello, world!
      - heading "Unicode characters" [level=3]
      - text: There is two non-breakable spaces between A and B.
      - heading "Numbers" [level=3]
      - text: 1 2 3
      - heading "Booleans" [level=3]
      - text: false true
      - heading "Keywords" [level=3]
      - text: :foo :bar :foo/bar
      - heading "Vectors" [level=3]
      - text: "[:div {:style {:color \\\"lime\\\"}} 1 2]"
      - 'heading "\`nil\` values" [level=3]'
    - article:
      - heading "Image" [level=2]
      - img
    - article:
      - heading "Vcup elements with funny names" [level=2]
      - text: :span#id1.class1.class2 :#id2.class3 :.class4
    - article:
      - heading "Fragments" [level=2]
      - text: 1a1b2a2b
    - article:
      - heading "Component with arguments" [level=2]
      - code: (is 123 (+ a b c))
    `);
});
