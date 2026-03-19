// @ts-check

import mdx from '@astrojs/mdx';
import sitemap from '@astrojs/sitemap';
import { defineConfig } from 'astro/config';
import remarkMath from 'remark-math';
import rehypeKatex from 'rehype-katex';
import mermaid from 'astro-mermaid';

// https://astro.build/config
export default defineConfig({
	site: 'https://mohanrajendran.github.io',
	integrations: [
		mdx(), 
		sitemap(), 
		mermaid({
			mermaidConfig: {
				theme: 'default',
				themeVariables: {
					fontFamily: "'DM Sans', sans-serif",
				}
			}
		})
	],
	markdown: {
		remarkPlugins: [remarkMath],
		rehypePlugins: [rehypeKatex],
		shikiConfig: {
			theme: 'dracula',
			wrap: true,
		},
	},
});
