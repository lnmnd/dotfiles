watch( './(.*).php' ) do |md|
  result = `./vendor/bin/phpunit`
  puts result

  if result.match(/FAILURES\!/) or result.match(/Fatal error/)
    failures = result.scan(/failure:\n\n(.*)\n/)
    system "notify-send 'Tests failed' #{failures[0]} -t 6000"
  end
end
