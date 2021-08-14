var count = 0;
require('http')
	.createServer((req, res) => res.end(String(count++)))
	.listen(8000);
