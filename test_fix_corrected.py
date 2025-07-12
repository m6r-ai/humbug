def test_copy_file_success_new_destination(self, custom_path_resolver, mock_authorization):
    """Test successful file copying to new destination."""
    # Create custom resolver that handles both source and destination paths
    path_mapping = {
        "source.txt": ("/test/mindspace/source.txt", "source.txt"),
        "dest.txt": ("/test/mindspace/dest.txt", "dest.txt")
    }
    resolver = custom_path_resolver(path_mapping=path_mapping)
    filesystem_tool = AIToolFileSystem(resolve_path=resolver)

    with patch('pathlib.Path.exists') as mock_exists, \
         patch('pathlib.Path.is_file') as mock_is_file, \
         patch('pathlib.Path.stat') as mock_stat, \
         patch('pathlib.Path.mkdir'), \
         patch('shutil.copy2') as mock_copy2:

        # Mock exists to return True for source, False for destination
        # Track which call we're on - first call is source, second is destination
        call_count = [0]
        def exists_side_effect():
            call_count[0] += 1
            # First call is for source (should exist), second is for destination (should not exist)
            return call_count[0] == 1

        mock_exists.side_effect = exists_side_effect
        mock_is_file.return_value = True

        mock_stat_result = MagicMock()
        mock_stat_result.st_size = 100
        mock_stat.return_value = mock_stat_result

        result = asyncio.run(filesystem_tool.execute(
            {"operation": "copy_file", "path": "source.txt", "destination": "dest.txt"},
            mock_authorization
        ))

        assert "File copied successfully: source.txt -> dest.txt" in result
        mock_copy2.assert_called_once()
        # Verify authorization was called with destructive=False for new destination
        mock_authorization.assert_called_once()
        args = mock_authorization.call_args[0]
        assert args[3] is False  # destructive parameter