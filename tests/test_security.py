"""
Comprehensive tests for Security module.
Achieves 100% code coverage for src/server/security.py
"""
import pytest
from unittest.mock import Mock, MagicMock
from flask import Flask, request, jsonify
from src.server.security import (
    add_security_headers,
    validate_request_size,
    validate_json_request,
    configure_cors,
    sanitize_input
)


class TestAddSecurityHeaders:
    """Test add_security_headers function"""
    
    def test_add_security_headers_basic(self):
        """Test adding security headers to response"""
        response = Mock()
        response.headers = {}
        
        result = add_security_headers(response)
        
        assert result is response
        assert response.headers['X-Content-Type-Options'] == 'nosniff'
        assert response.headers['X-Frame-Options'] == 'DENY'
        assert response.headers['X-XSS-Protection'] == '1; mode=block'
        assert 'max-age=31536000' in response.headers['Strict-Transport-Security']
        assert 'Referrer-Policy' in response.headers
        assert 'Permissions-Policy' in response.headers
    
    def test_add_security_headers_csp(self):
        """Test Content-Security-Policy header"""
        response = Mock()
        response.headers = {}
        
        add_security_headers(response)
        
        csp = response.headers['Content-Security-Policy']
        assert "default-src 'self'" in csp
        assert "cdn.socket.io" in csp
        assert "script-src" in csp
        assert "style-src" in csp
    
    def test_add_security_headers_hsts(self):
        """Test Strict-Transport-Security header"""
        response = Mock()
        response.headers = {}
        
        add_security_headers(response)
        
        hsts = response.headers['Strict-Transport-Security']
        assert 'max-age=31536000' in hsts
        assert 'includeSubDomains' in hsts
    
    def test_add_security_headers_permissions_policy(self):
        """Test Permissions-Policy header"""
        response = Mock()
        response.headers = {}
        
        add_security_headers(response)
        
        perms = response.headers['Permissions-Policy']
        assert 'geolocation=()' in perms
        assert 'microphone=()' in perms
        assert 'camera=()' in perms


class TestValidateRequestSize:
    """Test validate_request_size decorator"""
    
    def test_validate_request_size_within_limit(self):
        """Test request within size limit passes"""
        app = Flask(__name__)
        
        @app.route('/test', methods=['POST'])
        @validate_request_size(max_size=1000)
        def test_endpoint():
            return jsonify({"status": "ok"})
        
        with app.test_request_context('/test', method='POST', 
                                       headers={'Content-Length': '500'}):
            response = test_endpoint()
            data = response.get_json()
            assert data["status"] == "ok"
    
    def test_validate_request_size_exceeds_limit(self):
        """Test request exceeding size limit is rejected"""
        app = Flask(__name__)
        
        @app.route('/test', methods=['POST'])
        @validate_request_size(max_size=1000)
        def test_endpoint():
            return jsonify({"status": "ok"})
        
        with app.test_client() as client:
            # Send large payload that exceeds limit
            response = client.post('/test', 
                                   data='x' * 2000,
                                   content_type='application/json')
            assert response.status_code == 413
            data = response.get_json()
            assert 'error' in data
            assert 'too large' in data['error'].lower()
    
    def test_validate_request_size_no_content_length(self):
        """Test request without content-length header passes"""
        app = Flask(__name__)
        
        @app.route('/test', methods=['POST'])
        @validate_request_size(max_size=1000)
        def test_endpoint():
            return jsonify({"status": "ok"})
        
        with app.test_request_context('/test', method='POST'):
            response = test_endpoint()
            data = response.get_json()
            assert data["status"] == "ok"
    
    def test_validate_request_size_default_16mb(self):
        """Test default max size is 16MB"""
        app = Flask(__name__)
        
        @app.route('/test', methods=['POST'])
        @validate_request_size()  # Default 16MB
        def test_endpoint():
            return jsonify({"status": "ok"})
        
        with app.test_client() as client:
            # 10KB should pass (well under 16MB)
            response = client.post('/test',
                                   data='x' * (10 * 1024),
                                   content_type='application/json')
            data = response.get_json()
            assert data["status"] == "ok"
            
            # 20MB should fail
            response = client.post('/test',
                                   data='x' * (20 * 1024 * 1024),
                                   content_type='application/json')
            assert response.status_code == 413
    
    def test_validate_request_size_custom_limit(self):
        """Test custom size limit"""
        app = Flask(__name__)
        
        @app.route('/test', methods=['POST'])
        @validate_request_size(max_size=500)
        def test_endpoint():
            return jsonify({"status": "ok"})
        
        with app.test_client() as client:
            response = client.post('/test',
                                   data='x' * 600,
                                   content_type='application/json')
            data = response.get_json()
            assert response.status_code == 413
            assert data['max_size'] == 500


class TestValidateJsonRequest:
    """Test validate_json_request decorator"""
    
    def test_validate_json_request_post_with_json(self):
        """Test POST request with JSON content-type passes"""
        app = Flask(__name__)
        
        @app.route('/test', methods=['POST'])
        @validate_json_request
        def test_endpoint():
            return jsonify({"status": "ok"})
        
        with app.test_request_context('/test', method='POST',
                                       content_type='application/json',
                                       data='{"test": "data"}'):
            response = test_endpoint()
            data = response.get_json()
            assert data["status"] == "ok"
    
    def test_validate_json_request_post_without_json(self):
        """Test POST request without JSON content-type is rejected"""
        app = Flask(__name__)
        
        @app.route('/test', methods=['POST'])
        @validate_json_request
        def test_endpoint():
            return jsonify({"status": "ok"})
        
        with app.test_request_context('/test', method='POST',
                                       content_type='text/plain',
                                       data='plain text'):
            result = test_endpoint()
            if isinstance(result, tuple):
                response, status_code = result
            else:
                response, status_code = result, 200
            data = response.get_json()
            assert status_code == 415
            assert 'error' in data
            assert 'application/json' in data['error']
    
    def test_validate_json_request_put_with_json(self):
        """Test PUT request with JSON content-type passes"""
        app = Flask(__name__)
        
        @app.route('/test', methods=['PUT'])
        @validate_json_request
        def test_endpoint():
            return jsonify({"status": "ok"})
        
        with app.test_request_context('/test', method='PUT',
                                       content_type='application/json',
                                       data='{"test": "data"}'):
            response = test_endpoint()
            data = response.get_json()
            assert data["status"] == "ok"
    
    def test_validate_json_request_put_without_json(self):
        """Test PUT request without JSON is rejected"""
        app = Flask(__name__)
        
        @app.route('/test', methods=['PUT'])
        @validate_json_request
        def test_endpoint():
            return jsonify({"status": "ok"})
        
        with app.test_request_context('/test', method='PUT',
                                       content_type='text/html'):
            result = test_endpoint()
            if isinstance(result, tuple):
                response, status_code = result
            else:
                response, status_code = result, 200
            assert status_code == 415
    
    def test_validate_json_request_patch_with_json(self):
        """Test PATCH request with JSON content-type passes"""
        app = Flask(__name__)
        
        @app.route('/test', methods=['PATCH'])
        @validate_json_request
        def test_endpoint():
            return jsonify({"status": "ok"})
        
        with app.test_request_context('/test', method='PATCH',
                                       content_type='application/json',
                                       data='{"test": "data"}'):
            response = test_endpoint()
            data = response.get_json()
            assert data["status"] == "ok"
    
    def test_validate_json_request_patch_without_json(self):
        """Test PATCH request without JSON is rejected"""
        app = Flask(__name__)
        
        @app.route('/test', methods=['PATCH'])
        @validate_json_request
        def test_endpoint():
            return jsonify({"status": "ok"})
        
        with app.test_request_context('/test', method='PATCH',
                                       content_type='application/x-www-form-urlencoded'):
            result = test_endpoint()
            if isinstance(result, tuple):
                response, status_code = result
            else:
                response, status_code = result, 200
            assert status_code == 415
    
    def test_validate_json_request_get_allowed(self):
        """Test GET request is allowed without JSON"""
        app = Flask(__name__)
        
        @app.route('/test', methods=['GET'])
        @validate_json_request
        def test_endpoint():
            return jsonify({"status": "ok"})
        
        with app.test_request_context('/test', method='GET'):
            response = test_endpoint()
            data = response.get_json()
            assert data["status"] == "ok"
    
    def test_validate_json_request_delete_allowed(self):
        """Test DELETE request is allowed without JSON"""
        app = Flask(__name__)
        
        @app.route('/test', methods=['DELETE'])
        @validate_json_request
        def test_endpoint():
            return jsonify({"status": "ok"})
        
        with app.test_request_context('/test', method='DELETE'):
            response = test_endpoint()
            data = response.get_json()
            assert data["status"] == "ok"


class TestConfigureCors:
    """Test configure_cors function"""
    
    def test_configure_cors_wildcard_default(self):
        """Test CORS with default wildcard"""
        app = Flask(__name__)
        configure_cors(app)
        
        @app.route('/test')
        def test_endpoint():
            return jsonify({"status": "ok"})
        
        with app.test_client() as client:
            response = client.get('/test')
            assert response.headers['Access-Control-Allow-Origin'] == '*'
            assert 'Access-Control-Allow-Methods' in response.headers
            assert 'Access-Control-Allow-Headers' in response.headers
    
    def test_configure_cors_explicit_wildcard(self):
        """Test CORS with explicit wildcard"""
        app = Flask(__name__)
        configure_cors(app, allowed_origins='*')
        
        @app.route('/test')
        def test_endpoint():
            return jsonify({"status": "ok"})
        
        with app.test_client() as client:
            response = client.get('/test')
            assert response.headers['Access-Control-Allow-Origin'] == '*'
    
    def test_configure_cors_specific_origin_allowed(self):
        """Test CORS with specific allowed origin"""
        app = Flask(__name__)
        configure_cors(app, allowed_origins=['https://example.com'])
        
        @app.route('/test')
        def test_endpoint():
            return jsonify({"status": "ok"})
        
        with app.test_client() as client:
            response = client.get('/test', headers={'Origin': 'https://example.com'})
            assert response.headers.get('Access-Control-Allow-Origin') == 'https://example.com'
    
    def test_configure_cors_specific_origin_denied(self):
        """Test CORS denies unlisted origin"""
        app = Flask(__name__)
        configure_cors(app, allowed_origins=['https://example.com'])
        
        @app.route('/test')
        def test_endpoint():
            return jsonify({"status": "ok"})
        
        with app.test_client() as client:
            response = client.get('/test', headers={'Origin': 'https://evil.com'})
            # Origin header should not be set for disallowed origins
            assert response.headers.get('Access-Control-Allow-Origin') != 'https://evil.com'
    
    def test_configure_cors_multiple_origins(self):
        """Test CORS with multiple allowed origins"""
        app = Flask(__name__)
        configure_cors(app, allowed_origins=['https://example.com', 'https://app.example.com'])
        
        @app.route('/test')
        def test_endpoint():
            return jsonify({"status": "ok"})
        
        with app.test_client() as client:
            response1 = client.get('/test', headers={'Origin': 'https://example.com'})
            assert response1.headers.get('Access-Control-Allow-Origin') == 'https://example.com'
            
            response2 = client.get('/test', headers={'Origin': 'https://app.example.com'})
            assert response2.headers.get('Access-Control-Allow-Origin') == 'https://app.example.com'
    
    def test_configure_cors_allowed_methods(self):
        """Test CORS sets allowed methods"""
        app = Flask(__name__)
        configure_cors(app)
        
        @app.route('/test')
        def test_endpoint():
            return jsonify({"status": "ok"})
        
        with app.test_client() as client:
            response = client.get('/test')
            methods = response.headers['Access-Control-Allow-Methods']
            assert 'GET' in methods
            assert 'POST' in methods
            assert 'PUT' in methods
            assert 'DELETE' in methods
            assert 'OPTIONS' in methods
    
    def test_configure_cors_allowed_headers(self):
        """Test CORS sets allowed headers"""
        app = Flask(__name__)
        configure_cors(app)
        
        @app.route('/test')
        def test_endpoint():
            return jsonify({"status": "ok"})
        
        with app.test_client() as client:
            response = client.get('/test')
            headers = response.headers['Access-Control-Allow-Headers']
            assert 'Content-Type' in headers
            assert 'Authorization' in headers
    
    def test_configure_cors_max_age(self):
        """Test CORS sets max age"""
        app = Flask(__name__)
        configure_cors(app)
        
        @app.route('/test')
        def test_endpoint():
            return jsonify({"status": "ok"})
        
        with app.test_client() as client:
            response = client.get('/test')
            assert response.headers['Access-Control-Max-Age'] == '3600'
    
    def test_configure_cors_from_env(self, monkeypatch):
        """Test CORS configuration from environment variable"""
        import os
        monkeypatch.setenv('CORS_ORIGINS', 'https://site1.com,https://site2.com')
        
        app = Flask(__name__)
        configure_cors(app, allowed_origins=None)
        
        @app.route('/test')
        def test_endpoint():
            return jsonify({"status": "ok"})
        
        with app.test_client() as client:
            response = client.get('/test', headers={'Origin': 'https://site1.com'})
            assert response.headers.get('Access-Control-Allow-Origin') == 'https://site1.com'


class TestSanitizeInput:
    """Test sanitize_input function"""
    
    def test_sanitize_input_normal_text(self):
        """Test sanitizing normal text"""
        result = sanitize_input("Hello World")
        assert result == "Hello World"
    
    def test_sanitize_input_removes_html_tags(self):
        """Test sanitizing removes HTML tag characters"""
        result = sanitize_input("<script>alert('xss')</script>")
        assert '<' not in result
        assert '>' not in result
        # Quotes are removed, so (xss) becomes result without quotes
        assert "script" in result
        assert "alert" in result
        assert "xss" in result
    
    def test_sanitize_input_removes_quotes(self):
        """Test sanitizing removes quotes"""
        result = sanitize_input('He said "hello" and \'goodbye\'')
        assert '"' not in result
        assert "'" not in result
    
    def test_sanitize_input_removes_ampersand(self):
        """Test sanitizing removes ampersand"""
        result = sanitize_input("Tom & Jerry")
        assert '&' not in result
        assert result == "Tom  Jerry"
    
    def test_sanitize_input_removes_semicolon(self):
        """Test sanitizing removes semicolon"""
        result = sanitize_input("SELECT * FROM users; DROP TABLE users;")
        assert ';' not in result
    
    def test_sanitize_input_removes_pipe(self):
        """Test sanitizing removes pipe character"""
        result = sanitize_input("cmd | grep pattern")
        assert '|' not in result
    
    def test_sanitize_input_removes_backtick(self):
        """Test sanitizing removes backtick"""
        result = sanitize_input("Execute `malicious_command`")
        assert '`' not in result
    
    def test_sanitize_input_truncates_long_text(self):
        """Test sanitizing truncates text over max length"""
        long_text = "A" * 2000
        result = sanitize_input(long_text, max_length=1000)
        assert len(result) == 1000
    
    def test_sanitize_input_custom_max_length(self):
        """Test sanitizing with custom max length"""
        text = "Short text"
        result = sanitize_input(text, max_length=5)
        assert len(result) == 5
        assert result == "Short"
    
    def test_sanitize_input_strips_whitespace(self):
        """Test sanitizing strips leading/trailing whitespace"""
        result = sanitize_input("  Hello World  ")
        assert result == "Hello World"
    
    def test_sanitize_input_non_string(self):
        """Test sanitizing converts non-string to string"""
        result = sanitize_input(12345)
        assert result == "12345"
        
        result = sanitize_input(None)
        assert result == "None"
    
    def test_sanitize_input_complex_injection(self):
        """Test sanitizing complex injection attempt"""
        injection = "<img src='x' onerror='alert(\"XSS\")' />"
        result = sanitize_input(injection)
        assert '<' not in result
        assert '>' not in result
        assert '"' not in result
        assert "'" not in result
    
    def test_sanitize_input_sql_injection(self):
        """Test sanitizing SQL injection attempt"""
        sql = "'; DROP TABLE users; --"
        result = sanitize_input(sql)
        assert "'" not in result
        assert ';' not in result
    
    def test_sanitize_input_command_injection(self):
        """Test sanitizing command injection attempt"""
        cmd = "test | cat /etc/passwd"
        result = sanitize_input(cmd)
        assert '|' not in result
    
    def test_sanitize_input_empty_string(self):
        """Test sanitizing empty string"""
        result = sanitize_input("")
        assert result == ""
    
    def test_sanitize_input_whitespace_only(self):
        """Test sanitizing whitespace-only string"""
        result = sanitize_input("   \t\n  ")
        assert result == ""
    
    def test_sanitize_input_unicode(self):
        """Test sanitizing unicode characters"""
        result = sanitize_input("Hello 世界 🌍")
        assert "Hello" in result
        assert "世界" in result
        assert "🌍" in result
    
    def test_sanitize_input_preserves_safe_chars(self):
        """Test sanitizing preserves safe characters"""
        safe_text = "Hello, World! 123 (test) [array] {dict} = + - * / % @ # $ _"
        result = sanitize_input(safe_text)
        # Should preserve most characters except dangerous ones
        assert "Hello" in result
        assert "World" in result
        assert "123" in result
        assert "test" in result
