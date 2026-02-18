import { getCollection } from 'astro:content';
import rss from '@astrojs/rss';

export async function GET(context) {
	const posts = await getCollection('blog');
	return rss({
		title: 'Mohan Rajendran',
		description: 'Senior Software Engineer at Amazon sharing insights on distributed systems, AI/ML, and computer science fundamentals.',
		site: context.site,
		items: posts.map((post) => ({
			...post.data,
			link: `/blog/${post.id}/`,
		})),
	});
}
