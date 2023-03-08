<template>
	<a :href="href" class="btn btn-primary" style="width: 150px">{{ text }}</a>
</template>

<script>
export default {
	name: "Button",
	prop: {
		text: String,
		href: String
	}
}

</script>
