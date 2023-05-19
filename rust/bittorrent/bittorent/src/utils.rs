pub fn urlencode(in_str: &[u8]) -> String {
    let mut escaped_info_hash = String::new();
    for byte in in_str {
        if byte.is_ascii_alphanumeric() || [b'.', b'-', b'_', b'~'].contains(&byte) {
            escaped_info_hash.push(*byte as char);
        } else {
            let str = format!("%{:x}", byte);
            escaped_info_hash.push_str(&str);
        };
    }
    escaped_info_hash
}
